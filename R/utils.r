library(data.table)
library(zoo)

# STRAT CV -----------------------------------------------------------------------------------------

sample2 <- function(x, size) if (length(x) == 1) return(x) else sample(x, size)

# MANEJO DE LISTAS ---------------------------------------------------------------------------------

#' Separa Lista Baseado Em Argumentos De Funcao
#' 
#' Retorna duas listas separando `list` entre argumentos de `fun` e demais elementos originais

match_fun_args <- function(list, fun) {
    fun_args <- formalArgs(fun)
    paired <- list[names(list) %in% fun_args]
    remain <- list[!(names(list) %in% fun_args)]

    return(list(paired, remain))
}

#' Merge Duas Listas
#' 
#' Combina duas listas de elementos nomeados, privilegiando `list1` no caso de repeticoes

merge_lists <- function(list1, list2) {
    merged <- c(list1, list2)
    dups <- duplicated(names(merged))
    merged <- merged[!dups]

    return(merged)
}

# UPSAMPLING ---------------------------------------------------------------------------------------

#' Upsample De Dados
#' 
#' Rebaixa um dado \code{x} para resolucao \code{times} vezes menor
#' 
#' Se \code{times = 2}, preenche uma nova observacao interpolada entre cada duas originais; se 
#' \code{times = 3}, preenche duas novas observacoes e assim por diante. Assim, para upsample de
#' um dado de 3 em 3 horas para meia em meia hora, deve ser usado \code{times = 6}
#' 
#' @param x um vetor ou data.frame-like para upsample
#' @param times frequencia de upsample, inteiro maior que 2. Veja Detalhes
#' @param type metodo de preenchimento. Atualmente sao suportados "linear", "cubic"
#' @param expand_right booleano indicando se deve ser extrapolado a direita
#' @param time_col se \code{x} for data.frame-like, o nome da coluna de tempo na qual basear o
#'     upsample
#' @param value_col se \code{x} for data.frame-like, o nome da coluna de dados para upsample
#' @param ... demais argumentos passados para as internas de interpolacao
#' 
#' @examples 
#' 
#' # caso simples de upsample de um vetor
#' x_num <- c(1, 4, -5, 10, 7)
#' x_num_upsampled <- upsample(x, 3, "linear")
#' 
#' # com um data.table
#' dt <- data.table(data = as.Date("2020-01-01") + seq(1, 13, 3), valor = c(1, 4, -5, 10, 7))
#' dt_upsampled <- upsample(dt, 3, "linear", time_col = "data", value_col = "valor")
#' 
#' @return objeto \code{x} upsampled para resolucao desejada

upsample <- function(x, times, type = c("linear", "cubic"), expand_right = FALSE, ...) UseMethod("upsample")

#' @rdname upsample

upsample.numeric <- function(x, times, type = c("linear", "cubic"), expand_right = FALSE, ...) {
    type <- match.arg(type)
    fun  <- str2lang(paste0("upsample_", type))
    upsampled <- match.call()
    upsampled[[1]] <- fun
    upsampled[["type"]] <- NULL
    upsampled <- eval(upsampled, parent.frame(), parent.frame())

    return(upsampled)
}

#' @rdname upsample

upsample.Date <- function(x, times, type = c("linear", "cubic"), expand_right = FALSE, ...) {
    out <- upsample.numeric(as.numeric(x), times, type, expand_right, ...)
    out <- as.Date(out)
    return(out)
}

#' @rdname upsample

upsample.POSIXt <- function(x, times, type = c("linear", "cubic"), expand_right = FALSE, ...) {
    ref <- x[1]
    out <- upsample.numeric(as.numeric(x), times, type, expand_right, ...)
    out <- as.POSIXct(out, attr(ref, "tzone"))
    return(out)
}

#' @rdname upsample

upsample.list <- function(x, times, time_col, value_col,
    type = c("linear", "cubic"), expand_right = FALSE, ...) {

    time_col_ds  <- upsample(x[[time_col]], times, "linear", expand_right, ...)
    value_col_ds <- upsample(x[[value_col]], times, type, expand_right, ...)

    out <- list(time_col_ds, value_col_ds)
    names(out) <- c(time_col, value_col)

    return(out)
}

#' @rdname upsample

upsample.data.frame <- function(x, times, time_col, value_col,
    type = c("linear", "cubic"), expand_right = FALSE, ...) {

    out <- upsample.list(x, times, time_col, value_col, type, expand_right, ...)
    out <- as.data.frame(out)
    return(out)
}

#' @rdname upsample

upsample.data.table <- function(x, times, time_col, value_col,
    type = c("linear", "cubic"), expand_right = FALSE, by = "", ...) {

    out <- x[,
        upsample.list(
            .(get(time_col), get(value_col)),
            times, 1, 2, type, expand_right, ...),
        by = by]

    names <- names(out)
    names[names == "1"] <- time_col
    names[names == "2"] <- value_col
    names(out) <- names

    return(out)
}

upsample_linear <- function(x, times, expand_right, ...) {

    if (all(is.na(x))) {
        fill <- ifelse(inherits(x, "numeric"), NA_real_, NA_integer_)
        out <- rep(fill, length(x) * times)
        return(out)
    }

    xout <- seq(1, length(x), times^-1)
    inner <- approx(seq_along(x), x, xout, rule = 2, ...)$y

    if (expand_right) {
        x_outer <- c(tail(x, 1), tail(x, 1) + diff(tail(x, 2)))
        outer <- approx(seq_along(x_outer), x_outer, seq(1, 2, times^-1), rule = 2, ...)$y
        outer <- outer[-c(1, length(outer))]
        inner <- c(inner, outer)
    }

    return(inner)
}

# BUILD DE REGRESSORES -----------------------------------------------------------------------------

#' Gera Slices De Dado Observado
#' 
#' Auxiliar para gerar e transformar slices de uma variavel observada

build_lagged_slice <- function(dt, value_col, max_lag,
    start_time = "07:30:00", time_col = "datahora",
    transform = function(x) dwt(x, value_col, filter = "haar")) {

    start <- get_start(start_time, head(dt[[time_col]], 48))
    slice <- slice(dt, value_col, time_col, L = seq(-max_lag + 1, 0),
        start = start, step = 48)
    slice <- transform(slice)

    return(slice)
}

#' Repete Um Slice Para Dia Seguinte
#' 
#' Replica um slice 48 vezes, modificando seu indice a cada vez para uma semihora do dia seguinte

replicate_slice_day_ahead <- function(slice) {
    hours <- generate_hours()
    slice <- force_slice_hour(slice, "00:00:00")
    slice <- lapply(hours, function(hour) {
        slice_i <- offset_slice_time(slice, "1 day")
        slice_i <- force_slice_hour(slice_i, hour)
        slice_i
    })
    slice <- Reduce(c, slice)

    return(slice)
}

#' Gera Vetor De Horas String A Cada Meia Hora

generate_hours <- function() {
    hours <- outer(c("00", "30"), formatC(seq(0, 23), flag = "0", width = 2), function(a, b) {
        paste0(b, ":", a, ":00")
    })
    c(hours)
}

#' Deslocamento De Indice De Slice
#' 
#' Desloca o indice de um slice de montante igual a \code{offset}
#' 
#' @param slice slice cujo indice sera deslocado
#' @param offset numero ou string (ex: "1 day", "3 weeks", etc.) indicando o offset

offset_slice_time <- function(slice, offset) {
    index <- attr(slice, "index")
    new_index <- lapply(index, function(i) seq(i, length.out = 2, by = offset))
    new_index <- sapply(new_index, tail, 1)
    new_index <- as.POSIXct(new_index, tz = attr(index[1], "tzone"))

    attr(slice, "index") <- new_index
    return(slice)
}

#' Forca Indice De Slice Para Hora Especifica
#' 
#' Forca o horario de todos os indices num slice para \code{target_hour}, mantendo datas

force_slice_hour <- function(slice, target_hour) {
    index <- attr(slice, "index")
    target_hour_num <- hora_str2num(target_hour)
    hours <- nhour(index)
    offset <- target_hour_num - hours
    new_index <- index + (offset * 3600)

    attr(slice, "index") <- new_index
    return(slice)
}

#' Subset Em Janela Rolante De Slices
#' 
#' Gera slice ampliado a partir de \code{x} por subsets de largura \code{window} em seus elementos

rolling_subset <- function(x, window) {
    indexes <- attr(x, "index")
    out <- lapply(indexes, function(i) {
        x_i <- x[, i]
        single_index_subset(x_i, window)
    })
    out <- Reduce(c, out)
    return(out)
}

#' Subset Rolante Em Unico Elemento Em Slice
#' 
#' Auxiliar interna para \code{rolling_subset}

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

#' Separa Uma Janela De Temperatura
#' 
#' Separa tamanho de janela \code{L} em máximo lag observado e máximo lead previsto

split_l_temp <- function(hora_execucao, L, roll) {
    hora_num <- hora_str2num(hora_execucao)

    max_lead_prev <- (47.5 - hora_num) * 2
    min_lead_prev <- (24 - hora_num) * 2
    max_lag_obs   <- L - ifelse(roll, min_lead_prev, max_lead_prev)

    split_l <- c(max_lag_obs, max_lead_prev)
    return(split_l)
}

#' Converte Hora String Em Numero

hora_str2num <- function(str) {
    str <- as.numeric(strsplit(str, ":")[[1]])
    str[2] <- str[2] / 60
    num <- str[1] + str[2]
    return(num)
}

#' Identifica Primeira Linha De \code{dt} Com Hora \code{hora}

get_start <- function(hora, vec) {
    horas <- format(vec, "%H:%M:%S")
    start <- grep(hora, horas)
    return(start)
}

# ESCALONAMENTO ------------------------------------------------------------------------------------

#' Wrappers De \code{scale} Melhorados
#' 
#' Facilitadores para padronizar e reescalar dado de maneira mais automatica
#' 
#' @param dt data.table para padronizar ou reescalar
#' @param ignore_classes classes de colunas que serao ignoradas na padronizacao
#' @param reference_dt data.table previamente padronizado por esta funcao para usar de referencia na
#'     padronizacao/reescalonamento de \code{dt}

scale2 <- function(dt, reference_dt = NULL, ...) UseMethod("scale2", reference_dt)

scale2.NULL <- function(dt, reference_dt,
    ignore_classes = c("character", "integer", "POSIXt", "Date")) {

    ignore_cols <- lapply(dt, inherits, what = ignore_classes)
    means <- mapply(dt, ignore_cols, FUN = function(x, b) ifelse(b, 0, mean(x)))
    sds   <- mapply(dt, ignore_cols, FUN = function(x, b) ifelse(b, 1, sd(x)))
    scales <- mapply(means, sds, FUN = c, SIMPLIFY = FALSE)

    new_scaled_dt(dt, scales, ignore_cols)
}

scale2.scaled_dt <- function(dt, reference_dt) {

    recover <- recover_scale_ignored(dt, reference_dt)
    scales  <- recover[[1]]
    ignored <- recover[[2]]

    new_scaled_dt(dt, scales, ignored)
}

new_scaled_dt <- function(dt, scales, ignored) {
    scaled <- mapply(dt, scales, ignored,
        FUN = function(x, s, i) if (i) x else (x - s[1]) / s[2],
        SIMPLIFY = FALSE)

    scaled <- as.data.table(scaled)
    class(scaled) <- c("scaled_dt", class(dt))
    attr(scaled, "scales") <- scales
    attr(scaled, "ignored") <- ignored

    return(scaled)
}

#' @rdname scale2

rescale2 <- function(dt, reference_dt = NULL) {

    recover <- recover_scale_ignored(dt, reference_dt)
    scales  <- recover[[1]]
    ignored <- recover[[2]]

    rescaled <- mapply(dt, scales, ignored,
        FUN = function(x, s, i) if (i) x else x * s[2] + s[1],
        SIMPLIFY = FALSE)
    rescaled <- as.data.table(rescaled)

    return(rescaled)
}

recover_scale_ignored <- function(dt, reference_dt) {
    dt_is_scaled <- inherits(dt, "scaled_dt")
    has_ref_dt   <- !is.null(reference_dt)
    ref_dt_is_scaled  <- has_ref_dt && inherits(reference_dt, "scaled_dt")
    ref_has_all_names <- has_ref_dt && all(names(dt) %in% names(reference_dt))

    if (!dt_is_scaled) {
        if (!has_ref_dt) stop("'dt' nao foi padronizado por 'scale2' e nao ha 'reference_dt'")
        if (!ref_dt_is_scaled) stop("'reference_dt' nao foi escalonado por 'scale2'")
        if (!ref_has_all_names) stop("'reference_dt' nao possui todas as colunas de 'dt'")

        scales <- attr(reference_dt, "scales")
        scales <- lapply(names(dt), function(name) scales[[name]])
        names(scales) <- names(dt)
        ignored <- attr(reference_dt, "ignored")
        ignored <- lapply(names(dt), function(name) ignored[[name]])
        names(ignored) <- names(dt)
    } else {
        scales <- attr(dt, "scales")
        ignored <- attr(dt, "ignored")
    }

    return(list(scales, ignored))
}

#' Simplifica Referencia Para Reescalonamento
#' 
#' Retorna \code{reference_dt} porem sem dados, servindo apenas para uso como referencia posterior

hollow_reference <- function(reference_dt) reference_dt[0L]
