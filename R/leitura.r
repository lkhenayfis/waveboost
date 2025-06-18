
get_areas <- function(no_subsistemas = FALSE, conn = .CONEXAO_BANCO) {
    d <- getfromdb(conn, "areas")
    if (no_subsistemas) {
        d <- d[!grepl("Subsistema", nome_area)]
    }
    d
}

get_feriados <- function(areas, conn = .CONEXAO_BANCO) {
    d <- getfromdb(conn, "feriados", codigo_area = areas)
    d[, simples := rep(1L, .N)]
}

get_carga_observada <- function(areas, janela = "2020/2024", conn = .CONEXAO_BANCO) {
    getfromdb(conn, "carga_observada", codigo_area = areas, datahora = janela)
}

get_temperatura_observada <- function(areas, janela = "2020/2024", conn = .CONEXAO_BANCO) {
    d <- getfromdb(conn, "temperatura_observada", codigo_area = areas, datahora = janela)
    resample(d, 2, expand_right = TRUE)
}

get_temperatura_prevista <- function(areas, janela = "2020/2024", conn = .CONEXAO_BANCO) {
    d <- getfromdb(conn, "temperatura_prevista", codigo_area = areas, datahora_execucao = janela)
    resample(d, 2, expand_right = TRUE, by = "datahora_execucao")
}