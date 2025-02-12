library(dbrenovaveis)
library(LoadServices)
library(data.table)
library(arrow)
library(aws.s3)

le_carga_historica <- function(area, janela = "1900/3000", simplificado = TRUE) {
    area   <- valida_codigo_area(area)
    janela <- expande_janela(janela, as_date = TRUE)

    dt <- CGPeriodo(area, janela[1], janela[2], TRUE)
    setDT(dt)

    parse_coluna_date(dt, "dat_referencia")
    parse_coluna_posix(dt, "din_referenciautc")

    if (simplificado) dt <- dt[, .(cod_areacarga, din_referenciautc, val_cargaglobalcons)]

    return(dt)
}

le_temperatura_historica <- function(area, janela = "1900/3000") {
    area <- valida_codigo_area(area)
    id   <- area2id(area)
    janela <- expande_janela(janela, as_date = FALSE)

    dt <- TemperaturaPonderada(id, janela[1], janela[2])
    setDT(dt)

    parse_coluna_posix(dt, "DinOcorrencia")

    return(dt)
}

le_temperatura_prevista <- function(area, janela = "1900/3000", referencia = 1) {
    area <- valida_codigo_area(area)
    id   <- area2id(area)
    janela <- expande_janela(janela, as_date = FALSE)
    horizontes <- referencia2horizontes(referencia)

    dts <- lapply(horizontes, function(h) TemperaturaPrevistaDelay(id, janela[1], janela[2], h))
    dts <- lapply(dts, as.data.table)

    dts <- rbindlist(dts)
    dts[, DinReferencia := DinOrigem + referencia * (48 * 1800)]
    setcolorder(dts, c(1, 2, 5, 3, 4))

    parse_coluna_date(dts, "DinOrigem")
    parse_coluna_date(dts, "DinReferencia")
    parse_coluna_posix(dts, "DinOcorrencia")

    return(dts)
}

le_feriados <- function(area, janela = "1900/3000") {
    dt <- aws.s3::s3read_using(fread,
        object = paste0("/prevcarga/decks/", area, "/FERIADOS.csv.gz"),
        bucket = "s3://ons-dl-prod-containers")
    return(dt)
}

le_horarioverao <- function(area, janela = "1900/3000") {
    dt <- aws.s3::s3read_using(fread,
        object = paste0("/prevcarga/decks/", area, "/HORAVERAO.csv.gz"),
        bucket = "s3://ons-dl-prod-containers")
    dt[, data_inicio := as.Date(Data.inicial, format = "%d/%m/%Y")]
    dt[, data_fim := as.Date(Data.final, format = "%d/%m/%Y")]
    dt[, c("Data.inicial", "Data.final") := .(NULL, NULL)]
    return(dt)
}

# HELPERS ------------------------------------------------------------------------------------------

valida_codigo_area <- function(area) {
    existe_area <- area %in% .INFO_AREAS$codigo_area
    if (!existe_area) {
        stop("argumento 'area' nao existe na base de dados")
    }

    return(area)
}

area2id <- function(area) .INFO_AREAS[.INFO_AREAS$codigo_area == area, "id_serie"]

referencia2horizontes <- function(ref) ref + 0:1

expande_janela <- function(janela, as_date) {
    janela <- dbrenovaveis:::parsedatas(janela, "", FALSE)
    janela <- sapply(seq_len(2), function(i) janela[[i]][i])

    if (as_date) janela <- sub(" .*", "", janela)

    return(janela)
}

parse_coluna_date <- function(dt, coluna) {
    dt[, (coluna) := as.Date(get(coluna))]
    return(dt)
}

parse_coluna_posix <- function(dt, coluna) {
    dt[, (coluna) := as.POSIXct(get(coluna), "UTC", format = "%Y-%m-%dT%H:%M:%SZ")]
    return(dt)
}
