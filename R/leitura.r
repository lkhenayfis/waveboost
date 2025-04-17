library(dbrenovaveis)

get_areas <- function(conn = .CONEXAO_BANCO) {
    getfromdb(conn, "areas")
}

get_feriados <- function(areas, conn = .CONEXAO_BANCO) {
    d <- getfromdb(conn, "feriados", codigo_area = areas)
    d[, simples := rep(1L, .N)]
}

get_carga_observada <- function(areas, janela = "2020/2024", conn = .CONEXAO_BANCO) {
    getfromdb(conn, "carga_observada", codigo_area = areas, datahora = janela)
}

get_temperatura_observada <- function(areas, janela = "2020/2024", conn = .CONEXAO_BANCO) {
    getfromdb(conn, "temperatura_observada", codigo_area = areas, datahora = janela)
}

get_temperatura_prevista <- function(areas, janela = "2020/2024", conn = .CONEXAO_BANCO) {
    getfromdb(conn, "temperatura_prevista", codigo_area = areas, datahora_execucao = janela)
}