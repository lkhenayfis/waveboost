
#' Modelo Singleshot
#' 
#' Estimadores de modelos singleshot com variados conjuntos de regressores
#' 
#' @details 
#' 
#' ## `model_params`
#' 
#' O argumento `model_params` pode ser uma lista contendo os seguintes elementos, divididos em
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
#' Os demais elementos de `model_params` podem corresponder a qualquer um dos argumentos passados no
#' argumento `model_params` de [lightgbm::lgb.train()] ou [lightgbm::lgb.Dataset()]
#' 
#' Para facilitacao de testes mais padronizados, existe uma funcao [default_model_params_ego()] que
#' retorna uma lista completa de parametros.
#' 
#' ## `test_config`
#' 
#' Similar a `model_params`, `test_config` controla o tipo de teste que sera feito. Este argumento
#' deve ser uma lista contendo sempre o elemento `modo`, que pode ser um de `c("cv", "split")`.
#' 
#' Se `modo = "cv"`, configura-se validacao cruzada. Neste caso a lista deve conter mais um
#' argumento `nfolds`, indicando o numero de folds a ser utilizado. Cada fold e gerado
#' automaticamente de forma estratificada.
#' 
#' Se `modo = "split"`, outros dois argumentos necessarios devem existir:
#' 
#' * `size` ou `frac`: se `size`, numero de amostras; se `frac` numero entre 0 e 1 indicando a
#'     fracao do dado que deve ser retirada para teste
#' * `where`: um de `c("head", "tail")` indicando de qual ponta estes dados serao retirados
#' 
#' @param carga,temp_obs,temp_prev,feriados data.tables de dados para geracao dos regressores
#' @param model_params lista de parametros do modelo. Veja Detalhes
#' @param test_config lista configurando o tipo e parametros de teste do modelo. Veja Detalhes

EGO <- function(
    carga, temp_obs, temp_prev, feriados,
    model_params = default_model_params_ego("M"),
    test_config = default_test_config_ego("cv")
) {

    test_config  <- merge_lists(test_config,  default_test_config_ego("cv"))

    model_params <- merge_lists(model_params, default_model_params_ego("M"))
    model_params <- match_fun_args(model_params, build_regs_quant_singleshot)

    data <- do.call(build_cache_dataset_ego,
        list(carga, temp_obs, temp_prev, feriados, model_params[[1]]))

    # se model_params$trend == TRUE, tirar tendencia e reservar modelo
    # padronizar e reservar referencia da padronizacao
    # treinamento segundo configuracao de test_config
    # retornar objeto completo (modelo, reg_build_fun, trend, scales)

}

# AUXILIARES ---------------------------------------------------------------------------------------

#' Wrapper Para Leitura/Montagem De Dataset Para EGO
#' 
#' Busca versao cached de dado previamente montado e le, caso exista, ou monta e salva do contrario

build_cache_dataset_ego <- function(carga, temp_obs, temp_prev, feriados, model_params,
    cache_dir = Sys.getenv("CACHE_DIR", "./data/cache")) {

    range_datas <- range(carga$datahora)
    hash <- c(as.list(range_datas), model_params)
    hash <- rlang::hash(hash)

    file <- file.path(cache_dir, paste0(hash, ".parquet.gzip"))
    if (file.exists(file)) {
        data <- arrow::read_parquet(file)
    } else {
        data <- do.call(build_regs_quant_singleshot,
            c(list(carga, temp_obs, temp_prev, feriados), model_params[[1]]))
        arrow::write_parquet(data, file)
    }

    gc()
    return(data)
}

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

#' Auxiliar Para Configuracao De Teste
#' 
#' Helper para definicao de templates de parametros para teste de modelo EGO
#' 
#' O unico argumento desta funcao, `modo`, permite gerar listas de parametros templatizadas
#' mais facilmente. Existem duas opcoes
#' 
#' * `"cv"`: corresponde a validacao cruzada com 5 folds
#' * `"split"`: separacao do dado em treino/teste tradicional com treino nos 30% finais do dado
#'
#' @param modo um de `c("cv", "split")`. Veja Detalhes

default_test_config_ego <- function(modo = c("cv", "split")) {
    modo <- match.arg(modo)

    if (modo == "cv") {
        params <- list(nfolds = 5)
    } else {
        params <- list(frac = .3, where = "tail")
    }

    config <- c(list(modo = modo), params)

    return(config)
}
