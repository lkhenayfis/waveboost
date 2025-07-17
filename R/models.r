
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

EGO <- function(carga, temp_obs, temp_prev, feriados,
    model_params = list(num_leaves = 8, min_data_in_leaf = 100),
    test_config = list(modo = "cv", nfolds = 4),
    data_pipes = default_data_pipes(),
    ...) {

    areas <- carga[, unique(codigo_area)]

    parsed_data_pipes <- parse_pipes(data_pipes)
    data <- eval_pipes(parsed_data_pipes)

    data <- merge(data, carga[, .(datahora, cargaglobalcons)], by.x = "index", by.y = "datahora")

    data <- scale2(data)
    ref  <- hollow_reference(data)

    trend <- lm2(cargaglobalcons ~ index, data)
    data[, cargaglobalcons := cargaglobalcons - predict(trend, data)]

    model <- train_EGO(data, model_params, test_config, ...)

    new_EGO(model, model_params, test_config, parsed_data_pipes, trend, ref, areas)
}

new_EGO <- function(model, model_params, test_config, data_pipes, trend, scales,
    areas = "") {

    new <- list(
        model = model,
        model_params = model_params,
        test_config = test_config,
        data_pipes = data_pipes,
        trend = trend,
        scales = scales)
    class(new) <- "EGO"
    attr(new, "codigo_area") <- areas

    return(new)
}

# SALVAMENTO ---------------------------------------------------------------------------------------

#' Salva Modelos EGO
#' 
#' Wrapper para salvar modelos reduzindo seu tamanho
#' 
#' O componente `trend` de modelo guarda uma referencia ao ambiente de estimacao da regressao, onde
#' estao todos os dados usados no fit do modelo. Isto ocasiona objetos extremamente pessados na
#' serializacao para rds, tornando-se necessario limpar o dado primeiro

save_model <- function(model, file) {
    rm(list = ls(envir = attr(model$trend$terms, ".Environment")),
        envir = attr(model$trend$terms, ".Environment"))
    saveRDS(model, file)
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

train_EGO_SPLIT <- function(data, model_params, size, frac, where) {
    if (!missing("frac")) size <- floor(frac * nrow(data))

    if (where == "tail") {
        training_sample <- data[, seq(.N) < (.N - size + 1)]
    } else if (where == "head") {
        training_sample <- data[, seq(.N) > size]
    }

    dataset_train <- lgb.Dataset(
        data.matrix(data[training_sample, .SD, .SDcols = -c("index", "cargaglobalcons")]),
        label = data[training_sample, cargaglobalcons],
        params = model_params
    )

    dataset_test <- lgb.Dataset(
        data.matrix(data[!training_sample, .SD, .SDcols = -c("index", "cargaglobalcons")]),
        label = data[!training_sample, cargaglobalcons],
        params = model_params
    )

    model <- lgb.train(data = dataset_train, params = model_params,
        nrounds = 5000L, early_stopping_rounds = 50L, eval_freq = 100L,
        valids = list(valid = dataset_test))
    attr(model, "CV_score") <- model$best_score

    return(model)
}

#' Treinamento Testando Por Cross Validation
#' 
#' Variante de treino testando o modelo em `nfolds` conjuntos estratificados de validacao

train_EGO_CV <- function(data, model_params, nfolds, cv_results_only = FALSE, ...) {
    folds <- rep(rep(seq_len(nfolds), each = 7 * 48), length.out = nrow(data))
    folds <- split(seq_len(nrow(data)), folds)

    cat_vars <- sapply(data, inherits, what = "factor")
    cat_vars <- names(data)[cat_vars]
    model_params$categorical_variables <- cat_vars

    dataset <- lgb.Dataset(
        data.matrix(data[, .SD, .SDcols = -c("index", "cargaglobalcons")]),
        label = data$cargaglobalcons,
        params = model_params
    )

    CV <- lgb.cv(data = dataset, folds = folds, params = model_params,
        nrounds = 5000L, early_stopping_rounds = 50L, eval_freq = 100L)

    if (cv_results_only) return(c(CV$best_score, CV$best_iter))

    best_score <- CV$best_score
    best_iter  <- CV$best_iter
    rm("CV")
    gc()

    model <- lgb.train(data = dataset, nrounds = best_iter, params = model_params)
    attr(model, "CV_score") <- best_score

    return(model)
}

# PREVISAO -----------------------------------------------------------------------------------------

#' Previsao De Modelos EGO
#' 
#' Wrapper de previsao para modelos estimados via [`EGO()`]

predict.EGO <- function(object, carga, temp_obs, temp_prev, feriados, ...) {

    data <- eval_pipes(object$data_pipes)
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

#' Auxiliar Para Construcao De Pipes Default
#' 
#' Wrapper simples para definicao da lista de pipes padrao de modelo EGO

default_data_pipes <- function() {
    pipes <- list(
        list(
            on =  list("carga"),
            transforms =  list(
                list(
                    fun =  "gen_dwt_builder_lag",
                    var =  "cargaglobalcons", hora_execucao = "07:30:00", L = 64
                )
            )
        ),
        list(
            on =  list("carga", "feriados"),
            transforms =  list(
                list(
                    fun =  "gen_merger_feriado"
                )
            )
        ),
        list(
            on = list("carga"),
            transforms = list(
                list(
                    fun = "gen_timefeature_adder"
                )
            )
        ),
        list(
            on = list("temp_obs", "temp_prev"),
            transforms = list(
                list(
                    fun = "gen_dwt_builder_laglead",
                    var_x = "temperatura", var_y = "temperatura",
                    hora_execucao = "07:30:00", L = 128, roll = TRUE
                )
            )
        )
    )

    return(pipes)
}