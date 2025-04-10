#library(modprev)
library(lightgbm)
#library(shapeshiftr)
library(data.table)
library(zoo)
library(ggplot2)

source("R/utils.r")
source("R/leitura.r")
source("R/build_regressores.r")
source("R/cross_validation_utils.r")

options(arrow.unsafe_metadata = TRUE)

carga_obs <- get_carga_observada("SECO", "2020/2024")
temp_obs  <- get_temperatura_observada("SECO", "2020/2024")
temp_prev <- get_temperatura_prevista("SECO", "2020/2024")
feriados  <- get_feriados("SECO")



# temp prev

datas <- c(
    "2024-10-01",
    "2024-10-31"
)

ggplot(temp_prev[datahora_execucao %between% datas]) +
    geom_line(aes(datahora_previsao, temperatura)) +
    facet_wrap(~datahora_execucao, scales = "free_x")

# temp obs

datas <- c(
    "2024-10-01",
    "2024-11-01"
)

ggplot(temp_obs[datahora %between% datas]) +
    geom_line(aes(datahora, temperatura)) +
    facet_wrap(~format(datahora, format = "%Y%m%d"), scales = "free_x")
