library(jsonlite)

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
#' * `hora_execucao`: ultima hora do dia disponivel quando da previsao
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
    test_config = default_test_config_ego("cv")) {

    test_config  <- merge_lists(test_config,  default_test_config_ego("cv"))
    model_params <- merge_lists(model_params, default_model_params_ego("M"))

    split <- match_fun_args(model_params, build_regs_quant_singleshot)
    data <- do.call(build_cache_data_quant_ego, list(carga, temp_obs, temp_prev, split[[1]]))

    split <- match_fun_args(split[[2]], add_regs_quali)
    data  <- do.call(add_regs_quali, list(data, feriados, split[[1]]))

    data <- scale2(data)
    ref  <- hollow_reference(data)

    form  <- if (model_params$trend) cargaglobalcons ~ datahora else cargaglobalcons ~ 1
    trend <- lm(form, carga)
    data[, cargaglobalcons := residuals(trend)]

    model <- train_EGO(data, split[[2]], test_config)

    new_EGO(model, split[[2]], trend, ref)
}

new_EGO <- function(model, model_params, trend, scales) {
    new <- list(model = model, trend = trend, scales = scales, params = model_params)
    class(new) <- "EGO"

    return(new)
}

train_EGO <- function(data, model_params, test_config) {
    call <- list(str2lang(paste0("train_EGO", toupper(call$modo))))
    call <- c(call, test_config[!grepl("modo", names(test_config))])
    call$data <- quote(data)
    call$model_params <- quote(model_params)
    eval(as.call(call), parent.frame(), parent.frame())
}

train_EGO_CV <- function(data, model_params, nfolds, ...) {
    folds <- rep(rep(seq_len(nfolds), each = 7 * 48), length.out = nrow(data))
    folds <- split(seq_len(nrow(data)), folds)

    dataset <- lgb.Dataset(
        data.matrix(data[, -(1:2)]),
        label = data$cargaglobalcons,
        params = model_params
    )

    model <- lgb.cv(
        data = dataset,
        nrounds = 5000L,
        folds = folds,
        params = model_params,
        early_stopping_rounds = 50L
    )

    return(model)
}

predict.EGO <- function(object, carga, temp_obs, temp_prev, feriados, ...) {
    # montar regressores exatamente como no fit
    # previsao
    # reescalornar e retornar tendencia
}

# AUXILIARES ---------------------------------------------------------------------------------------

#' Wrapper Para Leitura/Montagem De Dataset Para EGO
#' 
#' Busca versao cached de dado previamente montado e le, caso exista, ou monta e salva do contrario

build_cache_data_quant_ego <- function(carga, temp_obs, temp_prev, model_params,
    cache_dir = Sys.getenv("CACHE_DIR", "./data/cache")) {

    hash <- generate_hash(carga, model_params)
    registry  <- check_data_registry(cache_dir)
    hash_read <- search_data_registry(hash, registry)

    if (length(hash_read) > 0) {
        file <- file.path(cache_dir, paste0(hash_read, ".parquet.gzip"))
        data <- arrow::read_parquet(file)
        data <- data[datahora %between% carga$datahora]
    } else {
        file <- file.path(cache_dir, paste0(hash, ".parquet.gzip"))
        data <- do.call(build_regs_quant_singleshot,
            c(list(carga, temp_obs, temp_prev), model_params))
        arrow::write_parquet(data, file)
        update_data_registry(hash, cache_dir)
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
#' * `hora_execucao = "07:30:00"`
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
        hora_execucao = "07:30:00",
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
