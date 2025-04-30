
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
#' @param ... apenas para uso interno

EGO <- function(
    carga, temp_obs, temp_prev, feriados,
    model_params = default_model_params_ego("M"),
    test_config = default_test_config_ego("cv"),
    ...) {

    test_config  <- merge_lists(test_config,  default_test_config_ego("cv"))
    model_params <- merge_lists(model_params, default_model_params_ego("M"))

    split <- match_fun_args(model_params, build_regs_quant_singleshot)
    data <- do.call(build_cache_data_quant_ego, list(carga, temp_obs, temp_prev, split[[1]]))

    split <- match_fun_args(split[[2]], add_regs_quali)
    data  <- do.call(add_regs_quali, c(list(data, feriados), split[[1]]))

    data <- merge(data, carga[, .(datahora, cargaglobalcons)], by.x = "index", by.y = "datahora")

    data <- scale2(data)
    ref  <- hollow_reference(data)

    form  <- if (model_params$trend) cargaglobalcons ~ index else cargaglobalcons ~ 1
    trend <- lm(form, data)
    data[, cargaglobalcons := residuals(trend)]

    model <- train_EGO(data, split[[2]], test_config, ...)

    new_EGO(model, model_params, trend, ref)
}

new_EGO <- function(model, model_params, trend, scales) {
    new <- list(model = model, trend = trend, scales = scales, params = model_params)
    class(new) <- "EGO"

    return(new)
}

# TREINAMENTO DE MODELO ----------------------------------------------------------------------------

#' Entrypoint Generico Para Treino
#' 
#' Despacha o treino dependendo do tipo de teste configurado em `test_config`
#' 
#' @param data dados preparados durante execucao de `EGO`
#' @param model_params lista de parametros para serem utilizados no argumento `params` de 
#'     [lightgbm::lgb.train()] ou [lightgbm::lgb.Dataset()]
#' @param test_config lista definindo tipo e parametrizacao de teste do modelo. Veja [EGO()]

train_EGO <- function(data, model_params, test_config, ...) {
    call <- match.call()
    call[[1]] <- str2lang(paste0("train_EGO_", toupper(test_config$modo)))
    call$test_config <- NULL

    test_args <- names(test_config)[!grepl("modo", names(test_config))]
    for (arg in test_args) call[[arg]] <- test_config[[arg]]

    eval(as.call(call), parent.frame(), parent.frame())
}

#' Treinamento Testando Por Cross Validation
#' 
#' Variante de treino testando o modelo em `nfolds` conjuntos estratificados de validacao

train_EGO_CV <- function(data, model_params, nfolds, cv_results_only = FALSE, ...) {
    folds <- rep(rep(seq_len(nfolds), each = 7 * 48), length.out = nrow(data))
    folds <- split(seq_len(nrow(data)), folds)

    dataset <- lgb.Dataset(
        data.matrix(data[, .SD, .SDcols = -c("index", "cargaglobalcons")]),
        label = data$cargaglobalcons,
        params = model_params
    )

    CV <- lgb.cv(data = dataset, folds = folds, params = model_params,
        nrounds = 5000L, early_stopping_rounds = 50L, eval_freq = 100L)

    if (cv_results_only) return(CV[c("best_score", "best_iter")])

    best_iter  <- CV$best_iter
    rm("CV")
    gc()

    model <- lgb.train(data = dataset, nrounds = best_iter, params = model_params)

    return(model)
}

# PREVISAO -----------------------------------------------------------------------------------------

#' Previsao De Modelos EGO
#' 
#' Wrapper de previsao para modelos estimados via [`EGO()`]

predict.EGO <- function(object, carga, temp_obs, temp_prev, feriados, ...) {

    split <- match_fun_args(object$params, build_regs_quant_singleshot)
    data <- do.call(build_cache_data_quant_ego, list(carga, temp_obs, temp_prev, split[[1]]))

    split <- match_fun_args(split[[2]], add_regs_quali)
    data  <- do.call(add_regs_quali, c(list(data, feriados), split[[1]]))

    data <- scale2(data, object$scales)

    pred <- predict(object$model, data.matrix(data[, .SD, .SDcols = -c("index")]))

    pred <- data.table(
        index = data$index,
        cargaglobalcons = pred + predict(object$trend, data)
    )
    pred <- rescale2(pred, object$scales)
    colnames(pred)[1] <- "datahora"

    return(pred)
}

# AUXILIARES ---------------------------------------------------------------------------------------

#' Wrapper Para Leitura/Montagem De Dataset Para EGO
#' 
#' Busca versao cached de dado previamente montado e le, caso exista, ou monta e salva do contrario
#' 
#' @param carga,temp_obs,temp_prev data.tables de dados para montagem dos regressores
#' @param params lista nomeada de argumentos para chamada de [`build_regs_quant_singleshot()`]

build_cache_data_quant_ego <- function(carga, temp_obs, temp_prev, params,
    cache_dir = Sys.getenv("CACHE_DIR", "./data/cache")) {

    hash <- generate_hash(carga, params)
    registry  <- check_data_registry(cache_dir)
    hash_read <- search_data_registry(hash, registry)

    if (length(hash_read) > 0) {
        file <- file.path(cache_dir, paste0(hash_read, ".parquet.gzip"))
        data <- arrow::read_parquet(file)
        data <- data[index %between% range(carga$datahora)]
    } else {
        file <- file.path(cache_dir, paste0(hash, ".parquet.gzip"))
        data <- do.call(build_regs_quant_singleshot,
            c(list(carga, temp_obs, temp_prev), params))
        arrow::write_parquet(data, file)
        update_data_registry(hash, cache_dir)
    }

    gc()
    return(data)
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
        params <- list(nfolds = 4)
    } else {
        params <- list(frac = .3, where = "tail")
    }

    config <- c(list(modo = modo), params)

    return(config)
}
