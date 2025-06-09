source("R/waveboost.r")

# --------------------------------------------------------------------------------------------------

PONTA <- c(18.5, 21.5)

# --------------------------------------------------------------------------------------------------

areas <- get_areas(TRUE)

carga <- lapply(areas$codigo_area, get_carga_observada, janela = "2024-10/2025-01")
carga <- rbindlist(carga)

preds <- list.files("out/previsoes/", pattern = ".csv$", full.names = TRUE)
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
    by = .(nome_modelo, codigo_area, mes = format(datahora, format = "%Y%m"))]
MAPE_DIA_ANO <- dd[,
    .(MAPE_DIA = mean(abs(erro) / cargaglobalcons_obs)),
    by = .(nome_modelo, codigo_area)]

MAPE_PONTA_MES <- dd[
    nhour(datahora) %in% PONTA,
    .(MAPE_PONTA = mean(abs(erro) / cargaglobalcons_obs)),
    by = .(nome_modelo, codigo_area, mes = format(datahora, format = "%Y%m"))]
MAPE_PONTA_ANO <- dd[
    nhour(datahora) %in% PONTA,
    .(MAPE_PONTA = mean(abs(erro) / cargaglobalcons_obs)),
    by = .(nome_modelo, codigo_area)]

MAPE_MES <- merge(MAPE_DIA_MES, MAPE_PONTA_MES, by = c("nome_modelo", "codigo_area", "mes"))
MAPE_ANO <- merge(MAPE_DIA_ANO, MAPE_PONTA_ANO, by = c("nome_modelo", "codigo_area"))

setorder(MAPE_MES, codigo_area, mes, nome_modelo)
setorder(MAPE_ANO, codigo_area, nome_modelo)

fwrite(MAPE_MES, "out/MAPE_MES.csv")
fwrite(MAPE_ANO, "out/MAPE_ANO.csv")

# --------------------------------------------------------------------------------------------------

area_subsist <- rbind(
    data.frame(subsistema = "S", codigo_area = c("SC", "PR", "RS")),
    data.frame(subsistema = "SECO",
        codigo_area = c("RO", "MG", "RJ", "ES", "MT", "AC", "SP", "MS", "GO", "DF"))
)

dd <- merge(dd, area_subsist, by = "codigo_area")
dd <- dd[, .(cargaglobalcons_obs = cargaglobalcons_obs, erro = mean(erro)),
    by = .(datahora, nome_modelo, subsistema)]

MAPE_DIA_MES <- dd[,
    .(MAPE_DIA = mean(abs(erro) / cargaglobalcons_obs)),
    by = .(nome_modelo, subsistema, mes = format(datahora, format = "%Y%m"))]
MAPE_DIA_ANO <- dd[,
    .(MAPE_DIA = mean(abs(erro) / cargaglobalcons_obs)),
    by = .(nome_modelo, subsistema)]

MAPE_PONTA_MES <- dd[
    nhour(datahora) %in% PONTA,
    .(MAPE_PONTA = mean(abs(erro) / cargaglobalcons_obs)),
    by = .(nome_modelo, subsistema, mes = format(datahora, format = "%Y%m"))]
MAPE_PONTA_ANO <- dd[
    nhour(datahora) %in% PONTA,
    .(MAPE_PONTA = mean(abs(erro) / cargaglobalcons_obs)),
    by = .(nome_modelo, subsistema)]

MAPE_MES <- merge(MAPE_DIA_MES, MAPE_PONTA_MES, by = c("nome_modelo", "subsistema", "mes"))
MAPE_ANO <- merge(MAPE_DIA_ANO, MAPE_PONTA_ANO, by = c("nome_modelo", "subsistema"))

setorder(MAPE_MES, subsistema, mes, nome_modelo)
setorder(MAPE_ANO, subsistema, nome_modelo)

fwrite(MAPE_MES, "out/MAPE_MES.csv")
fwrite(MAPE_ANO, "out/MAPE_ANO.csv")
