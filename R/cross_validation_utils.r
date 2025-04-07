

sample_stratified_folds <- function(index, nfolds = 5, block_by = yearmonth, split_in = 1) {

    blocked    <- block_samples(index, block_by, split_in)
    compressed <- compress_list(blocked)
    all_paths <- block2paths(blocked)

    folds <- list()
    for (f in seq_len(nfolds)) {
        fold <- sample_single_fold(all_paths)
        fold <- which(compressed %in% fold)
        folds <- c(folds, list(fold))
    }

    return(folds)
}

sample_single_fold <- function(paths) {
    all_H2  <- attr(paths, "H2")
    all_S   <- attr(paths, "S")

    steps <- expand.grid(S = all_S, H2 = all_H2)
    N <- nrow(steps)

    choices <- lapply(seq_len(N), function(i) sample_h2_in_paths(paths, steps$H2[i], steps$S[i]))
    choices <- lapply(seq_len(3), function(i) sapply(choices, function(j) j[i]))
    choices <- compress_list(choices)

    return(choices)
}

sample_h2_in_paths <- function(paths, h2 = 1, s = 1) {
    H1_has_h2 <- get_H1_with_H2(paths, h2, s)
    h1 <- sample2(H1_has_h2, 1)

    paths[(H1 == h1) & (H2 == h2) & (S == s), used := TRUE]

    chosen_path <- c(h1, h2, s)

    check_reset_paths(paths)
    return(chosen_path)
}

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
#' Por enquanto, apenas dois niveis de hierarquia sao suportados
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

block2paths <- function(blocks) {
    H1 <- unique(blocks[[1]])
    H2 <- unique(blocks[[2]])
    S  <- unique(blocks[[3]])

    H2_by1 <- lapply(H1, function(u) {
        H2_u <- blocks[[2]][blocks[[1]] == u]
        unique(H2_u)
    })

    all_paths <- unique(do.call(cbind, blocks))
    all_paths <- as.data.table(all_paths)
    names(all_paths) <- c("H1", "H2", "S")
    all_paths[, used := FALSE]

    attr(all_paths, "H1") <- H1
    attr(all_paths, "H2") <- H2
    attr(all_paths, "S")  <- S
    attr(all_paths, "H2_by_H1") <- H2_by1

    return(all_paths)
}

get_H1_with_H2 <- function(paths, h2 = 1, s = 1) {
    H2_by_H1 <- attr(paths, "H2_by_H1")
    has_H2   <- sapply(H2_by_H1, function(i) h2 %in% i)
    H1_with_h2 <- attr(paths, "H1")[has_H2]

    # garante que o proximo nivel h1 seja um dos menos utilizados ate entao
    unused_h1 <- paths[H1 %in% H1_with_h2, sum(used), by = H1]
    unused_h1 <- unused_h1[, V1 <= min(V1)]

    # evita escolher um h1 cujo par (h2,s) ja tenha sido usado anteriormente
    unused_h2_in_h1 <- paths[(H2 == h2) & (S == s), !used]

    available <- unused_h1 & unused_h2_in_h1
    if (sum(available) == 0) available <- unused_h1

    choices <- H1_with_h2[available]

    return(choices)
}

check_reset_paths <- function(paths) {
    all_used <- paths[, all(used)]
    if (all_used) paths[, used := FALSE]
}
