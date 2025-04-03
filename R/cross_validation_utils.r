
#' Agrupa Valores Datetime Em Blocos
#' 
#' Funcao auxiliar para validacao cruzada estratificada; bloca, e opcionalmente subdivide, por date
#' 
#' \code{block_by} deve retornar uma lista de vetores, cada vetor com o mesmo comprimento de 
#' \code{index}. Os elementos da lista representam, em ordem hierarquica crescente, classes a qual
#' cada elemento pertence. Por exemplo, a funcao padrao bloca \code{index} por ano e mes, retornando
#' uma lista de dois elementos na qual o primeiro contem o ano a qual cada observacao pertence e o
#' segundo o mes.
#' 
#' Note que isso exige valores "repetidos" no nivel mais detalhado da hierarquia. Por exemplo, na
#' funcao padrao o nivel mes pode possuir diversos valores 1, indicando janeiro, porem em anos
#' diferentes
#' 
#' @param index vetor de indices no tempo
#' @param block_by funcao a ser aplicada em \code{index} para gerar o agrupamento por tempo. Veja
#'     Detalhes
#' @param split_in inteiro indicando em quantos conjuntos cada block deve ser subdividido
#' 
#' @return vector of same length as \code{index} indicating to which class each index belongs

block_samples <- function(index, block_by = yearmonth, split_in = 4) {
    time_blocks  <- block_by(index)
    block_splits <- split_blocks(time_blocks, split_in)
    return(block_splits)
}

yearmonth <- function(v) list(year(v), month(v))

compress_list <- function(l) Reduce(paste0, l)

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
    reduced <- compress_list(blocks)
    reduced <- split(reduced, factor(reduced, levels = unique(reduced)))
    splits <- lapply(reduced, split_single_block, split_in = split_in)
    splits <- unname(unlist(splits))
    blocks <- c(blocks, list(splits))
    return(blocks)
}
