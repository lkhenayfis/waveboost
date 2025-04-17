
#' Modelos Singleshot
#' 
#' Estimadores de modelos singleshot com variados conjuntos de regressores
#' 
#' O argumento \code{params} pode ser uma lista contendo os seguintes elementos, divididos em
#' dois grupos. O primeiro grupo diz respeito a arquitetura do modelo, comecando com os tamanhos de
#' sequencias para encoding
#' 
#' \enumerate{
#' \item{L_carga}
#' \item{L_mmgd}
#' \item{L_headindex}
#' \item{L_temperatura}
#' }
#' 
#' Deve ser notado que os tres primeiros dizem respeito a valores observados, portanto tamanhos de 
#' sequencias recentes a serem incluidas no modelo. A sequencia de temperatura combinara, se
#' necessario, dados observados e previstos. Em seguida estao
#'                         
#' \enumerate{
#' \item{hora_inicio}{ultima hora do dia disponivel quando da previsao}
#' \item{trend}{booleano, se deve ser modelada tendencia linear no tempo}
#' \item{roll_temp}{booleano, se temperatura deve ser uma sequencia caminhando no tempo ou sempre a 
#'     mesma contemplando todo o trajeto ate o final do dia seguinte}
#' }
#' 
#' Os demais elementos de \code{params} podem corresponder a qualquer um dos argumentos passados no
#' argumento \code{params} de \code{\link[lightgbm]{lgb.train}} ou \code{\link[lightgbm]{lgb.Dataset}}
#' 
#' * S: 32 valores de carga e 64 de temperatura
#' * M: padrao, 64 valores de carga e 128 de temperatura
#' * L: 64 valores de carga, 64 valores de mmgd e 128 de temperatura
#' * XL: 64 valores de carga, 64 valores de mmgd, 64 valores de heatindex e 128 de temperatura
#' 
#' @param carga,temp_obs,temp_prev,feriados data.tables de dados para geracao dos regressores
#' @param model_params lista de parametros do modelo. Veja Detalhes

EGO <- function(
    carga, temp_obs, temp_prev, feriados,
    params = default_params_ego("M")
) {
    NA
}

# AUXILIARES ---------------------------------------------------------------------------------------

#' Reduz Lista Para Argumentos De Funcao
#' 
#' Retorna \code{list} apenas nas posicoes correspondentes aos argumentos de \code{fun}

match_fun_args <- function(list, fun) {

}

#' Auxiliar Para Construcao De Parametros
#' 
#' Helper para definicao de templates de parametros para modelo EGO

default_params_ego <- function(modo = c("M", "S", "L", "XL")) {
    modo    <- match.arg(modo)
    L_carga <- switch(modo, "S" = 32, "M" = 64, "L" = 64, "XL" = 64)
    L_mmgd  <- switch(modo, "S" =  0, "M" =  0, "L" = 64, "XL" = 64)
    L_heatindex   <- switch(modo, "S" =  0, "M" =   0, "L" =   0, "XL" = 64)
    L_temperatura <- switch(modo, "S" = 64, "M" = 128, "L" = 128, "XL" = 128)

    params <- list(L_carga, L_mmgd, L_heatindex, L_temperatura)
    return(params)
}