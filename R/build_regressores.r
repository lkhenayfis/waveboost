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

    reg_carga <- build_carga(carga_obs, hora_execucao, L_carga)
    reg_temp  <- build_temp(temp_obs, temp_prev, hora_execucao, L_temperatura, rolling)

    reg_carga <- as.data.table(reg_carga)
    reg_temp  <- as.data.table(reg_temp)
    reg <- merge(reg_carga, reg_temp)

    return(reg)
}

upsample_temperatura <- function(dt, tipo = c("obs", "prev")) {
    tipo <- match.arg(tipo)
    by       <- if (tipo == "obs") "codigo_area" else c("codigo_area", "datahora_execucao")
    time_col <- if (tipo == "obs") "datahora" else "datahora_previsao"

    upsample(dt, 2, time_col = time_col, value_col = "temperatura", by = by, expand_right = TRUE)
}

build_carga <- function(dt, hora_execucao, L) {
    start <- get_start(hora_execucao, head(dt$datahora, 48))

    out <- slice(dt, "carga", "datahora", L = seq(-L + 1, 0), start = start, step = 48)
    out <- dwt(out, "carga", filter = "haar")
    out <- lapply(seq_len(48), function(i) {
        delta <- 48 + i - start
        # corrige o indice para a hora alvo
        attr(out, "index") <- attr(out, "index") + delta * 1800
        out
    })
    out <- Reduce(c, out)

    return(out)
}

build_temp <- function(dt_obs, dt_prev, hora_execucao, L, roll) {

    split_L <- split_l_temp(hora_execucao, L, roll)
    start   <- get_start(hora_execucao, head(dt_obs$datahora, 48))

    obs <- slice(dt_obs, "temperatura", "datahora", L = seq(-split_L[1] + 1, 0),
        start = start, step = 48, names = "temp_obs")
    # rebaixa o indice para 00:00 do dia
    attr(obs, "index") <- attr(obs, "index") - (start - 1) * 30 * 60

    prev <- slice(dt_prev, "temperatura", "datahora_execucao", "datahora_previsao",
        seq(start, by = 1, length.out = split_L[2]), start = 1, names = "temp_prev")

    out <- merge(obs, prev)
    out <- combine_features(out, "temp_obs", "temp_prev")

    out <- rolling_subset(out, L)
    out <- dwt(out, "temp_obs_c_temp_prev", filter = "haar")

    return(out)
}

rolling_subset <- function(x, window) {
    indexes <- attr(x, "index")
    out <- lapply(indexes, function(i) {
        x_i <- x[, i]
        single_index_subset(x_i, window)
    })
    out <- Reduce(c, out)
    return(out)
}

single_index_subset <- function(x, window) {
    out <- rollapply(x$temp_obs_c_temp_prev[[1]], window, function(v) v)
    out <- lapply(seq_len(nrow(out)), function(i) out[i, ])
    out <- list(out)
    names(out) <- names(x)

    # corrige indice para a hora alvo, isto e, final daquele subset
    index <- attr(x, "index") + seq(0, 47) * (30 * 60) + (24 * 60 * 60)

    out <- shapeshiftr:::new_slice_artifact(out, index, NA)
    return(out)
}

split_l_temp <- function(hora_execucao, L, roll) {
    hora_num <- hora_str2num(hora_execucao)

    max_lead_prev <- (47.5 - hora_num) * 2
    min_lead_prev <- (24 - hora_num) * 2
    max_lag_obs   <- L - ifelse(roll, min_lead_prev, max_lead_prev)

    split_l <- c(max_lag_obs, max_lead_prev)
    return(split_l)
}

hora_str2num <- function(str) {
    str <- as.numeric(strsplit(str, ":")[[1]])
    str[2] <- str[2] / 60
    num <- str[1] + str[2]
    return(num)
}

get_start <- function(hora, vec) {
    horas <- format(vec, "%H:%M:%S")
    start <- grep(hora, horas)
    return(start)
}