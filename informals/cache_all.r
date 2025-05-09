source("R/leitura.r")
source("R/utils.r")
source("R/build_regressores.r")
source("R/data-registry.r")
source("R/models.r")

areas <- get_areas()
areas <- areas[!grepl("Subsistema", nome_area)]

model_params <- default_model_params_ego("M")

for (area in areas$codigo_area[24]) {
    print(area)
    carga_obs <- get_carga_observada(area, "2020/2024")
    temp_obs  <- get_temperatura_observada(area, "2020/2024")
    temp_prev <- get_temperatura_prevista(area, "2020/2024")

    null <- build_cache_data_quant_ego(carga_obs, temp_obs, temp_prev, model_params)
}
