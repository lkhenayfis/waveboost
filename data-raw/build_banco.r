library(LoadServices)
library(arrow)
library(data.table)

source("data-raw/leitores-loadservices.r")
source("creds-prevcarga.r")

AmbienteAtivo("PRD")
.INFO_AREAS <- areas <- fread("data/info_areas.csv")

# Roraima por algum motivo nao tem feriados // tambem e sistema isolado, nao precisa estar aqui
areas <- areas[codigo_area != "RR"]

datahora_inicio <- as.POSIXct("2020-01-01", "UTC")
datahora_fim <- as.POSIXct("2024-12-31 23:30:00", "UTC")

JANELA <- paste0(datahora_inicio - 24 * 60 * 60, "/", datahora_fim + 24 * 60 * 60)

# GERAL --------------------------------------------------------------------------------------------

schema <- list(
    application = "prevcarga-dessem",
    name = "banco-estudos-prevcarga",
    description = "Banco de dados para estudos em evolucoes metodologicas do prevcarga-dessem",
    version = "1.0",
    modifiedTime = Sys.time(),
    uri = "s3://ons-pem-historico/carga/estudos-prevcarga-dessem/",
    tables = list(
        list(name = "areas", uri = "./areas"),
        list(name = "feriados", uri = "./feriados"),
        list(name = "carga_observada", uri = "./carga_observada"),
        list(name = "temperatura_observada", uri = "./temperatura_observada"),
        list(name = "temperatura_prevista", uri = "./temperatura_prevista")
    )
)

jsonlite::write_json(schema, file.path("data", "banco", "schema.json"), auto_unbox = TRUE, pretty = TRUE)

# AREAS --------------------------------------------------------------------------------------------

uri <- "data/banco/areas"
dir.create(uri)
write_parquet(areas, file.path(uri, "areas.parquet.gzip"))

schema <- list(
    name = "areas",
    description = "tabela descritiva das areas eletricas, com nome, codigo e idspca",
    uri = "./areas",
    fileType = ".parquet.gzip",
    columns = list(
        list(name = "nome_area", type = "string"),
        list(name = "codigo_area", type = "string"),
        list(name = "id_serie", type = "int")
    ),
    partitions = list()
)

jsonlite::write_json(schema, file.path(uri, "schema.json"), auto_unbox = TRUE, pretty = TRUE)

# FERIADOS -----------------------------------------------------------------------------------------

uri <- "data/banco/feriados"
dir.create(uri)

feriados <- lapply(areas$codigo_area, get_feriados, janela = JANELA)
feriados <- rbindlist(feriados)
colnames(feriados) <- c("codigo_area", "data", "tipo_dia_especial", "codigo_prevcarga", "codigo_simplificado")
#feriados <- feriados[(data >= datahora_inicio) & (data <= datahora_fim)]
write_parquet(feriados, file.path(uri, "feriados.parquet.gzip"))

schema <- list(
    name = "feriados",
    description = "tabela descritiva dos feriados por area eletrica",
    uri = "./feriados",
    fileType = ".parquet.gzip",
    columns = list(
        list(name = "codigo_area", type = "string"),
        list(name = "data", type = "date"),
        list(name = "tipo_dia_especial", type = "int"),
        list(name = "codigo_prevcarga", type = "int"),
        list(name = "codigo_simplificado", type = "int")
    ),
    partitions = list()
)

jsonlite::write_json(schema, file.path(uri, "schema.json"), auto_unbox = TRUE, pretty = TRUE)

# CARGA OBS ----------------------------------------------------------------------------------------

uri <- "data/banco/carga_observada"
dir.create(uri)

for (area in areas$codigo_area) {
    carga <- get_carga_historica(area, JANELA, FALSE)
    carga[, dat_referencia := NULL]
    names(carga)[1:2] <- c("codigo_area", "datahora")
    names(carga) <- sub("^val_", "", names(carga))
    carga <- carga[(datahora >= datahora_inicio) & (datahora <= datahora_fim)]

    filename <- paste0("carga_observada-codigo_area=", area, ".parquet.gzip")
    write_parquet(carga, file.path(uri, filename))
}

schema <- list(
    name = "carga_observada",
    description = "tabela de historico da carga verificada",
    uri = "./carga_observada",
    fileType = ".parquet.gzip",
    columns = c(
        list(
            list(name = "codigo_area", type = "string"),
            list(name = "datahora", type = "datetime")
        ),
        lapply(names(carga)[-(1:2)], function(i) list(name = i, type = "float"))
    ),
    partitions = list(list(name = "codigo_area", type = "string"))
)

jsonlite::write_json(schema, file.path(uri, "schema.json"), auto_unbox = TRUE, pretty = TRUE)

# TEMPERATURA OBS ----------------------------------------------------------------------------------

uri <- "data/banco/temperatura_observada"
dir.create(uri)

for (area in areas$codigo_area) {
    temp <- get_temperatura_historica(area, JANELA)
    colnames(temp) <- c("id", "datahora", "temperatura")
    temp <- temp[(datahora >= datahora_inicio) & (datahora <= datahora_fim)]
    temp[, codigo_area := area]
    temp[, id := NULL]
    temp[, temperatura := as.numeric(temperatura)]
    setcolorder(temp, c("codigo_area", "datahora", "temperatura"))

    filename <- paste0("temperatura_observada-codigo_area=", area, ".parquet.gzip")
    write_parquet(temp, file.path(uri, filename))
}

schema <- list(
    name = "temperatura_observada",
    description = "tabela de historico da temperatura verificada",
    uri = "./temperatura_observada",
    fileType = ".parquet.gzip",
    columns = list(
        list(name = "codigo_area", type = "string"),
        list(name = "datahora", type = "datetime"),
        list(name = "temperatura", type = "float")
    ),
    partitions = list(list(name = "codigo_area", type = "string"))
)

jsonlite::write_json(schema, file.path(uri, "schema.json"), auto_unbox = TRUE, pretty = TRUE)

# TEMPERATURA PREV ---------------------------------------------------------------------------------

uri <- "data/banco/temperatura_prevista"
dir.create(uri)

for (area in areas$codigo_area) {
    temp <- get_temperatura_prevista(area, JANELA)
    temp <- temp[, .SD, .SDcols = c(1, 3, 4, 5)]
    colnames(temp) <- c("id", "datahora_execucao", "datahora_previsao", "temperatura")
    temp[, datahora_execucao := as.POSIXct(datahora_execucao, "UTC")]
    temp <- temp[(datahora_execucao >= datahora_inicio) & (datahora_execucao <= datahora_fim)]
    temp[, codigo_area := area]
    temp[, id := NULL]
    setcolorder(temp, c("codigo_area", "datahora_execucao", "datahora_previsao", "temperatura"))

    filename <- paste0("temperatura_prevista-codigo_area=", area, ".parquet.gzip")
    write_parquet(temp, file.path(uri, filename))
}

schema <- list(
    name = "temperatura_prevista",
    description = "tabela de historico da temperatura prevista para dia presente e seguinte",
    uri = "./temperatura_prevista",
    fileType = ".parquet.gzip",
    columns = list(
        list(name = "codigo_area", type = "string"),
        list(name = "datahora_execucao", type = "date"),
        list(name = "datahora_previsao", type = "datetime"),
        list(name = "temperatura", type = "float")
    ),
    partitions = list(list(name = "codigo_area", type = "string"))
)

jsonlite::write_json(schema, file.path(uri, "schema.json"), auto_unbox = TRUE, pretty = TRUE)
