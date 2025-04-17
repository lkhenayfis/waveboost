
#' Modelo Singleshot
#' 
#' Estimadores de modelos singleshot com variados conjuntos de regressores
#' 
#' @details 
#' 
#' ## `model_params`
#' 
#' O argumento `params` pode ser uma lista contendo os seguintes elementos, divididos em
#' dois grupos. O primeiro grupo diz respeito a arquitetura do modelo, comecando com os tamanhos de
#' sequencias para encoding
#' 
#' * `L_carga`
#' * `L_mmgd`
#' * `L_headindex`
#' * `L_temperatura`
#' 
#' Deve ser notado que os tres primeiros dizem respeito a valores observados, portanto tamanhos de 
#' sequencias recentes a serem incluidas no modelo. A sequencia de temperatura combinara, se
#' necessario, dados observados e previstos. Em seguida estao
#' 
#' * `hora_inicio`: ultima hora do dia disponivel quando da previsao
#' * `trend`: booleano, se deve ser modelada tendencia linear no tempo
#' * `roll_temp`: booleano, se temperatura deve ser uma sequencia caminhando no tempo ou sempre a 
#'     mesma contemplando todo o trajeto ate o final do dia seguinte
#' 
#' Os demais elementos de `params` podem corresponder a qualquer um dos argumentos passados no
#' argumento `params` de [lightgbm::lgb.train()] ou [lightgbm::lgb.Dataset()]
#' 
#' Para facilitacao de testes mais padronizados, existe uma funcao [default_model_params_ego()] que
#' retorna uma lista completa de parametros.
#' 
#' ## `test_params`
#' 
#' @param carga,temp_obs,temp_prev,feriados data.tables de dados para geracao dos regressores
#' @param model_params lista de parametros do modelo. Veja Detalhes
#' @param test_params lista configurando o tipo e parametros de teste do modelo. Veja Detalhes

EGO <- function(
    carga, temp_obs, temp_prev, feriados,
    model_params = default_model_params_ego("M"),
    test_params = default_test_params_ego()
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
#' 
#' O unico argumento desta funcao, `modo`, permite gerar listas de parametros templatizadas
#' mais facilmente. Cada valor de modo corresponde a seguinte configuracao de tamanhos de encoding
#' 
#' * S: 32 valores de carga e 64 de temperatura
#' * M: padrao, 64 valores de carga e 128 de temperatura
#' * L: 64 valores de carga, 64 valores de mmgd e 128 de temperatura
#' * XL: 64 valores de carga, 64 valores de mmgd, 64 valores de heatindex e 128 de temperatura
#' 
#' Em todos os casos a lista retornada contera os defaults
#' 
#' * `trend = TRUE`
#' * `hora_inicio = "07:30:00"`
#' * `roll = TRUE`
#'
#' @param modo um de `c("M", "S", "L", "XL")` indicando o tamanho do modelo. Veja Detalhes

default_model_params_ego <- function(modo = c("M", "S", "L", "XL")) {
    modo    <- match.arg(modo)
    L_carga <- switch(modo, "S" = 32, "M" = 64, "L" = 64, "XL" = 64)
    L_mmgd  <- switch(modo, "S" =  0, "M" =  0, "L" = 64, "XL" = 64)
    L_heatindex   <- switch(modo, "S" =  0, "M" =   0, "L" =   0, "XL" = 64)
    L_temperatura <- switch(modo, "S" = 64, "M" = 128, "L" = 128, "XL" = 128)

    params <- list(
        L_carga = L_carga,
        L_mmgd = L_mmgd,
        L_heatindex = L_heatindex,
        L_temperatura = L_temperatura,
        trend = TRUE,
        hora_inicio = "07:30:00",
        roll = TRUE
    )

    return(params)
}