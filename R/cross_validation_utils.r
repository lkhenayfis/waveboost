
#' Agrupa Valores Datetime Em Blocos
#' 
#' Funcao auxiliar para validacao cruzada estratificada; bloca, e opcionalmente subdivide, por date
#' 
#' @param index vetor de indices no tempo
#' @param block_by funcao a ser aplicada em \code{index} para gerar o agrupamento por tempo;
#'     padrao formata em ano-mes
#' @param split_in inteiro indicando em quantos conjuntos cada block deve ser subdividido
#' 
#' @return vector of same length as \code{index} indicating to which class each index belongs

block_samples <- function(index, block_by = yearmonth, split_in = 3) {
    time_blocks  <- block_by(index)
    block_splits <- split_blocks(time_blocks, split_in)
    return(block_splits)
}

yearmonth <- function(v) format(v, format = "%Y-%m")

split_single_block <- function(v, split_in) {
    N <- length(v)
    n_samples <- rep(ceiling(N / split_in), split_in)
    excess    <- sum(n_samples) - N
    subtract  <- c(rep(0, split_in - excess), rep(1, excess))
    n_samples <- n_samples - subtract
    classes <- unlist(lapply(seq_len(split_in), function(i) rep(i, each = n_samples[i])))
    return(classes)
}

split_blocks <- function(blocks, split_in) {
    blocks <- split(blocks, blocks)
    splits <- lapply(blocks, split_single_block, split_in = split_in)
    splitted <- mapply(blocks, splits, FUN = function(b, s) paste0(b, "_", s), SIMPLIFY = FALSE)
    splitted <- unname(unlist(splitted))
    return(splitted)
}
