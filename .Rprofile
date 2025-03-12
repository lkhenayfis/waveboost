source("renv/activate.R")

library(dbrenovaveis)

local({
    conn <- conectamock("s3://ons-pem-historico/carga/estudos-prevcarga-dessem/schema.json")
    assign(".CONEXAO_BANCO", conn, envir = .GlobalEnv)
})
