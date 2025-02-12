library(dbrenovaveis)
library(LoadServices)
library(data.table)

le_carga_historica <- function(area, janela = "1900/3000") {
    area   <- valida_codigo_area(area)
    janela <- expande_janela(janela, as_date = TRUE)

    dt <- CGPeriodo(area, janela[1], janela[2], TRUE)
    setDT(dt)

    parse_coluna_date(dt, "dat_referencia")
    parse_coluna_posix(dt, "din_referenciautc")

    return(dt)
}

le_temp_hist <- function(area, janela = "1900/3000") {
    dt <- leitor_dado_historico(area, janela, tipo = "TEMPHIST")
    return(dt)
}

le_temp_prev <- function(area, janela = "1900/3000", horizontes = seq(48)) {
    dt <- leitor_dado_historico(area, janela, tipo = "TEMPPREVHIST")

    dt[, h := din_referencia - as.POSIXct(din_origemprevisaoutc)]
    dt[, h := as.numeric(h / 3600)] # de segundos para horas à frente

    dt <- dt[h %in% horizontes]
    setorder(dt, din_origemprevisaoutc)

    return(dt)
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

# VALIDADADORES ------------------------------------------------------------------------------------

valida_codigo_area <- function(area) {
    existe_area <- area %in% .INFO_AREAS$codigo_area
    if (!existe_area) {
        stop("argumento 'area' nao existe na base de dados")
    }

    return(area)
}

# HELPERS ------------------------------------------------------------------------------------------

leitor_dado_historico <- function(area, janela, coluna = "din_referencia",
    tipo = c("CARGAHIST", "TEMPHIST", "TEMPPREVHIST", "FERIADOS", "HORAVERAO"),
    early = FALSE) {

    tipo <- match.arg(tipo)

    object <- paste0("/prevcarga/decks/", area, "/", tipo, ".csv.gz")
    dt <- aws.s3::s3read_using(fread, object = object, bucket = "s3://ons-dl-prod-containers")
    dt <- aplica_subset_data(dt, janela, coluna)

    if (early) return(dt)

    col_valor <- guess_col_valor(tipo)
    dt[, (col_valor) := as.numeric(sub(",", ".", get(col_valor)))]

    return(dt)
}

guess_col_valor <- function(tipo) {
    switch(tipo,
        CARGAHIST = "val_carga",
        TEMPHIST = "val_tmp",
        TEMPPREVHIST = "val_tmp")
}

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
    dt[, (coluna) := as.POSIXct(get(coluna), "America/SaoPaulo", format = "%Y-%m-%dT%H:%M:%SZ")]
    return(dt)
}
