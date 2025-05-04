library(logr)

source("R/waveboost.r")

# --------------------------------------------------------------------------------------------------

timestamp <- format(Sys.time(), format = "%Y%m%d_%H%M%S")
log_open(paste0("train_EGO_L_trend_", timestamp))

# --------------------------------------------------------------------------------------------------

month2window <- function(mes) {
    ano <- ifelse(mes == 1, 2023, 2024)
    mes <- ifelse(mes == 1, 12, mes - 1)
    mes <- formatC(mes, width = 2, flag = "0")
    paste0("2020/", ano, "-", mes)
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
    feature_pre_filter = FALSE,
    timefeatures = c("data.table::wday", "nhour")
)
MODEL_PARAMS_OPT <- list(
    min_data_in_leaf = c(25, 50, 100),
    max_leaves = c(16, 32)
)

CONFIGS <- generate_train_configs(MODEL_PARAMS, MODEL_PARAMS_OPT)

# --------------------------------------------------------------------------------------------------

areas <- get_areas(no_subsistema = TRUE)
LOOP <- expand.grid(mes = seq(9, 10), stringsAsFactors = FALSE, area = areas$codigo_area)

for (i in seq_len(nrow(LOOP))) {

    area <- LOOP[i, "area"]
    mes  <- LOOP[i, "mes"]

    log_print(paste(area, mes, sep = " -- "))

    arq <- paste0(NOME_MODELO, "-area=", area, "-mes=", mes, ".rds")
    arq <- file.path("out", "models", arq)

    carga_obs <- get_carga_observada(area, month2window(mes))
    temp_obs  <- get_temperatura_observada(area, month2window(mes))
    temp_prev <- get_temperatura_prevista(area, month2window(mes))
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
}
