
get_areas <- function(no_subsistemas = FALSE, conn = .CONEXAO_BANCO) {
    d <- getfromdb(conn, "areas")
    if (no_subsistemas) {
        d <- d[!grepl("Subsistema", nome_area)]
    }
    d
}

get_feriados <- function(areas, as_factor = TRUE, conn = .CONEXAO_BANCO) {
    d <- getfromdb(conn, "feriados", codigo_area = areas)
    d[, simples := rep(1L, .N)]
    if (as_factor) {
        d[] <- lapply(d,
            function(x) if (inherits(x, "integer")) factor(x, levels = unique(c(-1, x))) else x
        )
    }
    d
}

get_carga_observada <- function(areas, janela = "2020/2024", conn = .CONEXAO_BANCO) {
    getfromdb(conn, "carga_observada", codigo_area = areas, datahora = janela)
}

get_temperatura_observada <- function(areas, janela = "2020/2024", conn = .CONEXAO_BANCO) {
    d <- getfromdb(conn, "temperatura_observada", codigo_area = areas, datahora = janela)
    resample(d, 2, expand_right = TRUE)
}

get_temperatura_prevista <- function(areas, janela = "2020/2024", origem = "wrf_bam",
    max_h = 47.5, conn = .CONEXAO_BANCO) {

    d <- getfromdb(conn, "temperatura_prevista", origem = origem, codigo_area = areas,
        datahora_execucao = janela)
    d <- d[horizonte <= max_h]
    resample(d, 2, expand_right = TRUE, by = "datahora_execucao")
}