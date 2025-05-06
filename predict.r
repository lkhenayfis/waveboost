source("R/waveboost.r")

# --------------------------------------------------------------------------------------------------

dir.create("out/previsoes/", recursive = TRUE, showWarnings = FALSE)
file.remove(list.files("out/previsoes/", full.names = TRUE))

# --------------------------------------------------------------------------------------------------

get_model_metadata <- function(model) {
    model <- strsplit(model, "-")[[1]]

    kind <- sub(".*/", "", model[1])
    area <- sub("area=", "", model[2])
    data <- as.numeric(sub("mes=", "", sub(".rds", "", model[3])))
    data <- paste0(substr(data, 1, 4), "-", substr(data, 5, 6))

    list(kind, area, data)
}

# --------------------------------------------------------------------------------------------------

modelos <- list.files("out/models", full.names = TRUE)

# --------------------------------------------------------------------------------------------------

for (modelo in modelos) {
    meta <- get_model_metadata(modelo)
    nome <- meta[[1]]
    area <- meta[[2]]
    data <- meta[[3]]

    modelo <- readRDS(modelo)

    carga_obs <- get_carga_observada(area, data)
    temp_obs  <- get_temperatura_observada(area, data)
    temp_prev <- get_temperatura_prevista(area, data)
    feriados  <- get_feriados(area)

    pred <- predict(modelo, carga_obs, temp_obs, temp_prev, feriados)
    pred[, codigo_area := area]
    pred[, nome_modelo := nome]
    setorder(pred, datahora)
    setcolorder(pred, c("nome_modelo", "codigo_area"))

    out_arq <- paste0("carga_prevista",
        "-codigo_area=", area,
        "-nome_modelo=", nome,
        ".csv")
    out_arq <- file.path("out", "previsoes", out_arq)

    fwrite(pred, out_arq, append = file.exists(out_arq))
}
