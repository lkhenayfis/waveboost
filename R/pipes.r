
# PARSE DE PIPES -----------------------------------------------------------------------------------

#' Parse Um Unico Pipe
#' 
#' Interpreta a definicao de um unico pipe, gerando as closures definidas
#' 
#' @param raw_pipe uma lista definindo pipe singular, isto e, com elemento `"on"` e `"transforms"`
#' 
#' @return lista `raw_pipe` com elemento `"transforms"` avaliado para as closures definidas

parse_single_pipe <- function(raw_pipe) {
    raw_pipe$transforms <- lapply(raw_pipe$transforms, function(l) {
        l$fun <- str2lang(l$fun)
        cc <- as.call(l)
        eval(cc, parent.frame(), parent.frame())
    })
    return(raw_pipe)
}

#' Parse Lista De Pipes
#' 
#' Wrapper simples para loop de `parse_single_pipe` em multiplos pipes
#' 
#' @param raw_pipes lista definindo diversos pipes
#' 
#' @return lista `raw_pipes` com elementos `"transforms"` de cada pipe avaliados

parse_pipes <- function(raw_pipes) {
    lapply(raw_pipes, parse_single_pipe)
}

# EVAL DE PIPES ------------------------------------------------------------------------------------

#' Avalia Um Unico Pipe
#' 
#' Aplica as closures em um `pipe` já parsed ao dado definido em `"on"`
#' 
#' @param pipe um pipe parsed por `parse_single_pipe`
#' 
#' @return resultado da aplicacao das closures em `transforms` ao(s) dado(s) em `"on"`

eval_single_pipe <- function(pipe) {

    args <- lapply(pipe$on, str2lang)
    names(args) <- c("x", "y")[seq_along(args)]

    l_t <- pipe$transforms

    cc <- c(list(l_t[[1]]), args)
    x <- eval(as.call(cc), parent.frame(), parent.frame())
    l_t[[1]] <- NULL

    if (length(l_t) > 1) {
        for (f in seq_along(l_t)) {
            cc <- list(f, x)
            x <- eval(as.call(cc), parent.frame(), parent.frame())
        }
    }

    return(x)
}

#' Avalia Lista De Pipes
#' 
#' Wrapper simples para loop de `eval_single_pipe` em multiplos pipes
#' 
#' @param pipes lista de pipes ja parsed por `parse_pipes`
#' 
#' @return data.table unico combinando os resultados da aplicacao de todos os pipes em `pipes`

eval_pipes <- function(pipes) {
    evals <- lapply(pipes, eval_single_pipe)
    Reduce(function(x, y) merge(x, y, by.x = names(x)[1], by.y = names(y)[1]), evals)
}
