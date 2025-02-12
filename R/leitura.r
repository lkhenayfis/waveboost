library(dbrenovaveis)
library(LoadServices)
library(data.table)

le_carga_historica <- function(area, janela = "1900/3000") {
    area   <- valida_codigo_area(area)
    janela <- expande_janela(janela)

    dt <- CGPeriodo(area, janela[1], janela[2], TRUE)
    setDT(dt)

    dt[, dat_referencia := as.Date(dat_referencia)]
    dt[, din_referenciautc := as.POSIXct(din_referenciautc, "America/SaoPaulo",
            format = "%Y-%m-%dT%H:%M:%SZ")]

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

expande_janela <- function(janela) {
    janela <- dbrenovaveis:::parsedatas(janela, "", FALSE)
    janela <- sapply(seq_len(2), function(i) janela[[i]][i])
    janela <- sub(" .*", "", janela)
    return(janela)
}

aplica_subset_data <- function(dt, janela, coluna = "din_referencia") {
    janela <- expande_janela(janela)

    # se 'coluna' for uma coluna de data, tz vai voltar NULL
    # a estrategia de atribuir um elemento $tz na lista, quando tz e NULL, nao faz nada e passa sem
    # dar erro
    cc    <- list(as.POSIXct, janela)
    cc$tz <- attr(dt[[coluna]][1], "tzone")
    janela <- eval(as.call(cc))

    expr <- paste0(coluna, ">= janela[1] & ", coluna, "< janela[2]")
    expr <- str2expression(expr)
    dt <- dt[eval(expr)]

    return(dt)
}
