library(dbrenovaveis)
library(aws.s3)
library(arrow)
library(data.table)

le_carga_hist <- function(area, janela = "1900/3000") {
    dt <- leitor_dado_historico(area, janela, "CARGAHIST")
    return(dt)
}

le_temp_hist <- function(area, janela = "1900/3000") {
    dt <- leitor_dado_historico(area, janela, "TEMPHIST")
    return(dt)
}

le_temp_prev <- function(area, janela = "1900/3000", horizontes = seq(48)) {
    dt <- leitor_dado_historico(area, janela, "TEMPPREVHIST")

    dt[, h := din_referencia - as.POSIXct(din_origemprevisaoutc)]
    dt[, h := as.numeric(h / 3600)] # de segundos para horas à frente

    dt <- dt[h %in% horizontes]
    setorder(dt, h, din_referencia)

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

# HELPERS ------------------------------------------------------------------------------------------

leitor_dado_historico <- function(area, janela,
    tipo = c("CARGAHIST", "TEMPHIST", "TEMPPREVHIST", "FERIADOS", "HORAVERAO"),
    early = FALSE) {

    tipo <- match.arg(tipo)

    object <- paste0("/prevcarga/decks/", area, "/", tipo, ".csv.gz")
    dt <- aws.s3::s3read_using(fread, object = object, bucket = "s3://ons-dl-prod-containers")
    dt <- aplica_subset_data(dt, janela)

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

aplica_subset_data <- function(dt, janela) {
    janela <- dbrenovaveis:::parsedatas(janela, "", FALSE)
    janela <- sapply(seq(2), function(i) janela[[i]][i])

    dt <- dt[(din_referencia >= janela[1]) & (din_referencia < janela[2])]

    return(dt)
}
