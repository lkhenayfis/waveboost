library(shapeshiftr)
library(data.table)

source("R/utils.r")

# SINGLE SHOT --------------------------------------------------------------------------------------

#' Regressores Quantitativos Para Modelos Single Shot
#' 
#' Prepara regressores quantitativos para treinamento de modelos single shot
#' 
#' Esta funcao e responsavel por preparar os regressores quantitativos, isto e, wavelets dos 
#' trajetos de carga e temperatura, para treinamento dos modelos single shot.
#' 
#' \code{L_temperatura} representa o tamanho da janela de dados (obs e prev concatenados) de 
#' temperatura que serao utilizados para previsao. Se \code{rolling}, isto sera feito em janela que
#' termina sempre na semihora alvo de previsao. Do contrario, apenas uma janela, que vai até o final
#' do dia seguinte, sera utilizada para todas as horas.
#' 
#' @param carga_obs,temp_obs,temp_prev data.tables de dados observados e previstos
#' @param hora_execucao ultima hora para qual ha dados observados disponiveis
#' @param L_carga vetor de lags de carga a serem utilizados
#' @param L_temperatura vetor de lags/leads de temperatura a serem utilizados. Veja Detalhes
#'     transformados por wavelet
#' @param rolling booleano indicando se a temperatura deve ser tratada em janela rolante ou fixa
#' 
#' @return data.table indexado pela hora alvo de previsao contendo trajetos de carga e temperatura

build_regs_quant_singleshot <- function(
    carga_obs, temp_obs, temp_prev,
    hora_execucao = "07:30:00",
    L_carga = 64, L_temperatura = 128,
    rolling = TRUE) {

    temp_obs  <- upsample_temperatura(temp_obs, "obs")
    temp_prev <- upsample_temperatura(temp_prev, "prev")

    carga <- build_carga(carga_obs, hora_execucao, L_carga)
    temperatura <- build_temp(temp_obs, temp_prev, hora_execucao, L_temperatura, rolling)

    reg <- merge(carga, temperatura)

    return(reg)
}

upsample_temperatura <- function(dt, tipo = c("obs", "prev")) {
    tipo <- match.arg(tipo)
    by       <- if (tipo == "obs") "codigo_area" else c("codigo_area", "datahora_execucao")
    time_col <- if (tipo == "obs") "datahora" else "datahora_previsao"

    upsample(dt, 2, time_col = time_col, value_col = "temperatura", by = by, expand_right = TRUE)
}

build_carga <- function(dt, hora_execucao, L) {
    carga <- build_lagged_slice(dt, "cargaglobalcons", L, hora_execucao)
    carga <- replicate_slice_day_ahead(carga)
    carga <- as.data.table(carga)
    return(carga)
}

build_temp <- function(dt_obs, dt_prev, hora_execucao, L, roll) {

    split_L <- split_l_temp(hora_execucao, L, roll)
    start   <- get_start(hora_execucao, head(dt_obs$datahora, 48))

    temp <- slice(dt_prev, "temperatura", "datahora_execucao", "datahora_previsao",
        seq(start, by = 1, length.out = split_L[2]), start = 1, names = "temp_prev")

    if (split_L[1] >= 0) {
        obs <- slice(dt_obs, "temperatura", "datahora", L = seq(-split_L[1] + 1, 0),
            start = start, step = 48, names = "temp_obs")
        obs <- force_slice_hour(obs, "00:00:00")

        temp <- merge(obs, temp)
        temp <- combine_features(temp, "temp_obs", "temp_prev")
    }

    temp <- rolling_subset(temp, L)
    temp <- dwt(temp, names(temp)[1], filter = "haar")

    temp <- as.data.table(temp)

    return(temp)
}

#' Adiciona Regressores Qualitativos
#' 
#' Compoe adiciona informacao de feriados e timefeatures relevantes a regressores quantitativos
#' 
#' \code{timefeatures} deve ser um vetor de strings indicando funcoes a serem aplicadas a coluna de
#' indice de \code{reg}. Funcoes de pacotes especificos devem ser prefixadas de \code{"pacote::"}
#' para evitar erros. Funcoes custom tambem podem ser utilizadas, contanto que tenham sido 
#' carregadas no ambiente. Por padrao sao geradas features indicando dia na semana e hora numerica
#' 
#' @param reg data.table de regressores quantitativos ja montado por uma build_regs_quant
#' @param feriados opcional, data.table de feriados lido por \code{get_feriados}
#' @param pre_feriado,pos_feriado booleanos indicando se marcadores de dias antes e depois de 
#'     feriados devem ser adicionados
#' @param modo qual codificacao de feriados utilizar; um de
#'     \code{c("simples", "tipo_dia_especial", "codigo_prevcarga", "codigo_simplificado")}
#' @param timefeatures opcional, vetor de transformacoes de timestamp (NULL faz nada). Veja Detalhes

add_regs_quali <- function(reg, feriados = NULL,
    pre_feriado = TRUE, pos_feriado = TRUE,
    modo = c("simples", "tipo_dia_especial", "codigo_prevcarga", "codigo_simplificado"),
    timefeatures = c("data.table::wday", "nhour")) {

    if (!is.null(timefeatures)) {
        reg <- add_timefeatures(reg, timefeatures)
    }

    if (!is.null(feriados)) {
        reg <- add_feriados(reg, feriados, pre_feriado, pos_feriado, match.arg(modo))
    }

    return(reg)
}

add_feriados <- function(reg, feriados, pre, pos, modo) {
    feriados <- feriados[, .("date" = data, "feriado" = get(modo))]

    regdates <- reg[, .("date" = unique(as.Date(index)))]
    regdates <- merge(regdates, feriados, by = "date", all = TRUE)
    regdates[is.na(feriado), feriado := 0]

    if (pre) regdates[, pre_feriado := shift(feriado, -1)]
    if (pos) regdates[, pos_feriado := shift(feriado,  1)]

    reg[, merge_col := as.Date(index)]
    reg <- merge(reg, regdates, by.x = "merge_col", by.y = "date", all.x = TRUE)
    reg[, merge_col := NULL]

    return(reg)
}

add_timefeatures <- function(reg, timefeatures) {
    new <- lapply(timefeatures, function(f) {
        cc <- list(str2lang(f), reg$index)
        eval(as.call(cc))
    })
    names(new) <- sub("::", "_", timefeatures)
    new <- as.data.table(new)

    out <- cbind(reg, new)
    return(out)
}

nhour <- function(posix) hour(posix) + minute(posix) / 60 + second(posix) / 3600
