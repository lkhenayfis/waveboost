
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

upsample <- function(x, times, type = c("linear", "cubic"), ...) UseMethod("upsample")

#' @rdname upsample

upsample.numeric <- function(x, times, type = c("linear", "cubic"), ...) {
    type <- match.arg(type)
    fun    <- str2lang(paste0("upsample_", type))
    upsampled <- as.call(c(list(fun, x, times), list(...)))
    upsampled <- eval(upsampled, parent.frame(), parent.frame())

    return(upsampled)
}

#' @rdname upsample

upsample.Date <- function(x, times, type = c("linear", "cubic"), ...) {
    out <- upsample.numeric(as.numeric(x), times, type, ...)
    out <- as.Date(out)
    return(out)
}

#' @rdname upsample

upsample.POSIXt <- function(x, times, type = c("linear", "cubic"), ...) {
    ref <- x[1]
    out <- upsample.numeric(as.numeric(x), times, type, ...)
    out <- as.POSIXct(out, attr(ref, "tzone"))
    return(out)
}

#' @rdname upsample

upsample.list <- function(x, times, time_col, value_col,
    type = c("linear", "cubic"), ...) {

    time_col_ds  <- upsample(x[[time_col]], times, "linear", ...)
    value_col_ds <- upsample(x[[value_col]], times, type, ...)

    out <- list(time_col_ds, value_col_ds)
    names(out) <- c(time_col, value_col)

    return(out)
}

#' @rdname upsample

upsample.data.frame <- function(x, times, time_col, value_col,
    type = c("linear", "cubic"), ...) {

    out <- upsample.list(x, times, time_col, value_col, type, ...)
    out <- as.data.frame(out)
    return(out)
}

#' @rdname upsample

upsample.data.table <- function(x, times, time_col, value_col,
    type = c("linear", "cubic"), by = "", ...) {

    out <- x[,
        upsample.list(
            .(get(time_col), get(value_col)),
            times, 1, 2, type, ...),
        by = by]

    names <- names(out)
    names[names == "1"] <- time_col
    names[names == "2"] <- value_col
    names(out) <- names

    return(out)
}

upsample_linear <- function(x, times, ...) {
    approx(seq_along(x), x, seq(1, length(x), times^-1), ...)$y
}

upsample_cubic <- function(x, times, ...) {
    spline(seq_along(x), x, xout = seq(1, length(x), times^-1), ...)$y
}
