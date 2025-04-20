
#' Geracao De Hashes Para Dataset
#' 
#' Compoe uma hash baseado em parametros e datas utilizadas para compor um dataset EGO

generate_hash <- function(carga, model_params) {
    hash_elems <- c(
        codigo_area = list(carga$codigo_area[1]),
        range_datas = list(range(carga$datahora)),
        model_params)
    hash <- rlang::hash(hash_elems)
    attr(hash, "keys") <- hash_elems
    return(hash)
}

#' Formatacao De Hash Para Registro
#' 
#' Formata `hash` e seus parametros de geracao como lista para escrita no registro

format_hash_for_registry <- function(hash) c(list(hash = as.character(hash)), attr(hash, "keys"))

#' Escreve Registro
#' 
#' Wrapper para padronizacao das interacoes de escrita do registro de dados

write_data_registry <- function(registry = list(), cache_dir) {
    registry <- lapply(registry, function(r) {
        r$range_datas <- format(r$range_datas, format = "%FT%TZ%z")
        r
    })
    write_json(registry, file.path(cache_dir, "registry.json"), pretty = TRUE, auto_unbox = TRUE)
}

#' Le Registro
#' 
#' Wrapper para padronizacao das interacoes de leitura do registro de dados

read_data_registry <- function(cache_dir) {
    registry <- read_json(file.path(cache_dir, "registry.json"))
    registry <- lapply(registry, function(r) {
        r$range_datas <- as.POSIXct(unlist(r$range_datas), format = "%FT%TZ%z")
        r
    })
    return(registry)
}

#' Controle Para Novos Clones
#' 
#' Checa se existe um registro em `cache_dir` e, caso negativo, o cria

check_data_registry <- function(cache_dir) {
    file <- file.path(cache_dir, "registry.json")
    if (!file.exists(file)) write_data_registry(cache_dir = cache_dir)
    read_data_registry(cache_dir)
}

#' Atualizacao De Registro
#' 
#' Adiciona conteudos de `new`, uma hash criada por `generate_hash`, ao registro em `cache_dir`

update_data_registry <- function(new, cache_dir) {
    registry <- read_data_registry(cache_dir)
    registry <- c(registry, list(format_hash_for_registry(new)))
    write_data_registry(registry, cache_dir)
}

#' Busca De Elemento Compativel
#' 
#' Procura em `registry` o elemento coerente com `hash`; caso nao haja, retorna string vazia

search_data_registry <- function(hash, registry) {
    keys <- attr(hash, "keys")

    match_dates  <- sapply(registry, function(r) {
        all(as.numeric(keys$range_datas) %between% as.numeric(r$range_datas))
    })
    equal_params <- sapply(registry, function(r) {
        keys["range_datas"] <- NULL
        eq <- all(names(keys) %in% names(r))
        eq && all(mapply("==", keys, r[names(keys)]))
    })

    matches <- match_dates & equal_params

    if (sum(matches) != 1) {
        out <- character(0)
    } else {
        out <- registry[[matches]]$hash
    }

    return(out)
}
