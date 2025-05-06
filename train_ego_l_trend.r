library(logr)

source("R/waveboost.r")

# --------------------------------------------------------------------------------------------------

timestamp <- format(Sys.time(), format = "%Y%m%d_%H%M%S")
log_open(paste0("train_EGO_L_trend_", timestamp))

# --------------------------------------------------------------------------------------------------

parsedata <- function(data) {
    data <- as.Date(paste0(data, "-01"))
    data <- data - 1
    format(data, format = "2020/%Y-%m")
}

generate_train_configs <- function(model_params, model_params_opt) {
    model_params_opt <- do.call(expand.grid, model_params_opt)
    out <- lapply(seq_len(nrow(model_params_opt)), function(i) {
        c(model_params, as.list(model_params_opt[i, ]))
    })
}

# --------------------------------------------------------------------------------------------------

NOME_MODELO <- "WaveBoost_EGO_L_trend"
MODEL_PARAMS <- list(
    L_mmgd = 64,
    feature_pre_filter = FALSE,
    timefeatures = c("data.table::wday", "nhour")
)
MODEL_PARAMS_OPT <- list(
    min_data_in_leaf = c(50, 100, 200),
    max_leaves = c(8, 16, 32)
)

CONFIGS <- generate_train_configs(MODEL_PARAMS, MODEL_PARAMS_OPT)

# --------------------------------------------------------------------------------------------------

areas <- get_areas(no_subsistema = TRUE)
datas <- c("2024-10", "2024-11", "2024-12", "2025-01")
LOOP <- expand.grid(data = datas, area = areas$codigo_area, stringsAsFactors = FALSE)

log_print("Inicio do treinamento")

for (i in seq_len(nrow(LOOP))) {

    area <- LOOP[i, "area"]
    data <- LOOP[i, "data"]

    arq <- paste0(NOME_MODELO, "-area=", area, "-mes=", sub("\\-", "", data), ".rds")
    arq <- file.path("out", "models", arq)

    carga_obs <- get_carga_observada(area, parsedata(data))
    temp_obs  <- get_temperatura_observada(area, parsedata(data))
    temp_prev <- get_temperatura_prevista(area, parsedata(data))
    feriados  <- get_feriados(area)

    best_score <- Inf
    for (j in seq_along(CONFIGS)) {
        model <- EGO(carga_obs, temp_obs, temp_prev, feriados, CONFIGS[[j]])

        if (attr(model$model, "CV_score") < best_score) {
            best_score <- attr(model$model, "CV_score")
            save_model(model, arq)
        }

        rm(model)
        gc()
    }

    log_print(paste(area, data, sep = " -- "))
}

log_close()
