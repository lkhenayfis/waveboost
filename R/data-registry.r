
generate_hash <- function(carga, model_params) {
    hash_elems <- c(
        codigo_area = list(carga$codigo_area[1]),
        range_datas = list(range(carga$datahora)),
        model_params)
    hash <- rlang::hash(hash_elems)
    attr(hash, "keys") <- hash_elems
    return(hash)
}

format_hash_for_registry <- function(hash) c(list(hash = as.character(hash)), attr(hash, "keys"))

write_data_registry <- function(registry = list(), cache_dir) {
    registry <- lapply(registry, function(r) {
        r$range_datas <- format(r$range_datas, format = "%FT%TZ%z")
        r
    })
    write_json(registry, file.path(cache_dir, "registry.json"), pretty = TRUE, auto_unbox = TRUE)
}

read_data_registry <- function(cache_dir) {
    registry <- read_json(file.path(cache_dir, "registry.json"))
    registry <- lapply(registry, function(r) {
        r$range_datas <- as.POSIXct(unlist(r$range_datas), format = "%FT%TZ%z")
        r
    })
    return(registry)
}

check_data_registry <- function(cache_dir) {
    file <- file.path(cache_dir, "registry.json")
    if (!file.exists(file)) write_data_registry(cache_dir = cache_dir)
    read_data_registry(cache_dir)
}

update_data_registry <- function(new, cache_dir) {
    registry <- read_data_registry(cache_dir)
    registry <- c(registry, list(format_hash_for_registry(new)))
    write_data_registry(registry, cache_dir)
}

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
