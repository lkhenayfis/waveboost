source("R/waveboost.r")

# --------------------------------------------------------------------------------------------------

PONTA <- c(18.5, 21.5)

# --------------------------------------------------------------------------------------------------

areas <- get_areas(TRUE)

carga <- lapply(areas$codigo_area, get_carga_observada, janela = "2024")
carga <- rbindlist(carga)

preds <- list.files("out/previsoes/", full.names = TRUE)
preds <- lapply(preds, fread)
preds <- rbindlist(preds)

dd <- merge(
    carga[, .(codigo_area, datahora, cargaglobalcons)],
    preds,
    by = c("codigo_area", "datahora"),
    suffixes = c("_obs", "_prev")
)
dd[, erro := cargaglobalcons_obs - cargaglobalcons_prev]

MAPE_DIA_MES <- dd[,
    .(MAPE_DIA = mean(abs(erro) / cargaglobalcons_obs)),
    by = .(nome_modelo, codigo_area, month(datahora))]
MAPE_DIA_ANO <- dd[,
    .(MAPE_DIA = mean(abs(erro) / cargaglobalcons_obs)),
    by = .(nome_modelo, codigo_area)]

MAPE_PONTA_MES <- dd[
    nhour(datahora) %in% PONTA,
    .(MAPE_PONTA = mean(abs(erro) / cargaglobalcons_obs)),
    by = .(nome_modelo, codigo_area, month(datahora))]
MAPE_PONTA_ANO <- dd[
    nhour(datahora) %in% PONTA,
    .(MAPE_PONTA = mean(abs(erro) / cargaglobalcons_obs)),
    by = .(nome_modelo, codigo_area)]

MAPE_MES <- merge(MAPE_DIA_MES, MAPE_PONTA_MES, by = c("nome_modelo", "codigo_area", "month"))
MAPE_ANO <- merge(MAPE_DIA_ANO, MAPE_PONTA_ANO, by = c("nome_modelo", "codigo_area"))

setorder(MAPE_MES, codigo_area, month, nome_modelo)
setorder(MAPE_ANO, codigo_area, nome_modelo)

fwrite(MAPE_MES, "out/MAPE_MES.csv")
fwrite(MAPE_ANO, "out/MAPE_ANO.csv")