
# CLOSURE GENERATORS -------------------------------------------------------------------------------

#' Definicoes De Geradores De Closure
#' 
#' Funcoes que geram as closures utilizadas para preparo dos regressores em tempo de treino/previsao
#' 
#' Todas as funcoes definidas aqui são closure generators. Isto significa que nao operam no dado
#' diretamente, mas retornam uma nova funcao de um argumento (ou dois, em casos particulares) que
#' realiza o trabalho.
#' 
#' A intencao deste design e garantir que seja possivel replicar exatamente o mesmo pipeline de
#' manipulacao de dados tanto no momento de treino quanto de previsao, simplificando a
#' implementacao de novos modelos e variacoes de regressores, bem como teste e validacao de diversas
#' parametrizacoes.
#' 
#' ## Espeficicacoes
#' 
#' Todas as closures retornadas devem ter um, ou no maximo dois, argumento(s). Estes sao sempre os
#' `data.table`s nos quais a closure opera e serao um dos `data.table`s de dados disponiveis em 
#' tempo de execucao (carga, temperatura obs/prev e feriados).
#' 
#' De forma similar, todas elas devem retornar `data.table`s que seram combinados via `merge` para
#' construcao do dado de regressores finais. Este `merge` sera realizado pela primeira coluna de 
#' cada `data.table`, entao deve ser garantido pelo retorno da closure que esta seja a coluna 
#' correta.
#' 
#' ## Boas praticas
#' 
#' E importante notar que closures carregam seus ambientes onde foram declaradas consigo para que
#' possam encontrar nomes que existiam naquele escopo. Isto e uma caracteristica importante pois
#' e o que nos permite criar instancias de funcoes genericas parametrizadas dinamicamente, porem
#' pode ocasionar problemas de tamanho em memoria/disco apos o salvamento.
#' 
#' Tudo o que nao for necessario para a execucao da closure DEVE SER REMOVIDO DO AMBIENTE antes do
#' retorno.

# --------------------------------------------------------------------------------------------------

#' Gerador De Closure Para Encoder Lagged
#' 
#' Produz uma closure de unico argumento para realizar encoding dwt em dado passado

gen_dwt_builder_lag <- function(var = "", hora_execucao = "07:30:00", L = 64,
    time_col = "datahora") {

    fun <- function(x) {
        x <- build_lagged_slice(x, var, L, hora_execucao, time_col = time_col)
        x <- replicate_slice_day_ahead(x)
        x <- as.data.table(x)
        x
    }

    return(fun)
}

#' Gerador De Closure Para Encoder Lagged
#' 
#' Produz uma closure de unico argumento para realizar encoding dwt em dado passado

gen_dwt_builder_laglead <- function(var_x = "", var_y = "",
    hora_execucao = "07:30:00", L = 128, roll = TRUE) {

    fun <- function(x, y) {
        split_L <- split_l_temp(hora_execucao, L, roll)
        start   <- get_start(hora_execucao, head(x$datahora, 48))

        y <- slice(y, var_y, "datahora_execucao", "datahora_previsao",
            seq(start, by = 1, length.out = split_L[2]), start = 1,
            names = paste0(var_y, "_prev"))

        if (split_L[1] >= 0) {
            x <- slice(x, var_x, "datahora", L = seq(-split_L[1] + 1, 0),
                start = start, step = 48, names = paste0(var_x, "_obs"))
            x <- force_slice_hour(x, "00:00:00")

            out <- merge(x, y)
            out <- combine_features(out, paste0(var_x, "_obs"), paste0(var_y, "_prev"))
        }

        out <- rolling_subset(out, L, names(out[1]))
        out <- dwt(out, names(out)[1], filter = "haar")

        as.data.table(out)
    }

    return(fun)
}

gen_resampler <- function(times = 2, by = c()) {

    fun <- function(x) {
        resample(x, times, type = "linear", by, expand_right = TRUE)
    }

    return(fun)
}

#' Gerador De Merger Com Dado De Feriados
#' 
#' Produz closure de dois argumentos (dado alvo e calendario de feriados) para unir informacoes
#' 
#' Essa aqui esta um pouco mais marretada do que poderia ser, so vai funcionar se utilizada com 
#' dado de carga ou temperatura observada diretamente

gen_merger_feriado <- function(modo = "simples", pos = TRUE, pre = TRUE) {

    fun <- function(x, feriados) {
        feriados <- feriados[, .("date" = data, "feriado" = get(modo))]

        out <- x[, .(datahora, "date" = as.Date(datahora))]
        out <- merge(out, feriados, by = "date", all = TRUE)
        out[is.na(feriado), feriado := 0]

        if (pre) out[, pre_feriado := shift(feriado, -1)]
        if (pos) out[, pos_feriado := shift(feriado,  1)]

        out[, date := NULL]
        out <- out[datahora %between% range(x$datahora)]

        return(out)
    }

}