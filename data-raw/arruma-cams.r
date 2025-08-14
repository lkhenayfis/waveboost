library(aws.s3)
library(arrow)
library(data.table)

source("creds-cams.r")

area_grid <- fread("data-raw/areacargagrid.csv")
area_grid <- area_grid[Prioridade == 1]
area_grid <- area_grid[!duplicated(area_grid), .SD, .SDcols = -c(2, 3, 4, 7, 8)]
area_grid <- melt(area_grid, id.vars = 1)
area_grid <- dcast(area_grid, cod_areacarga ~ variable, fun.aggregate = mean)
names(area_grid) <- c("area", "lat", "lon")

# --------------------------------------------------------------------------------------------------

parse_name <- function(obj) {
    name <- obj$Key

    data_exec <- regmatches(name, regexpr("[[:digit:]]{8}", name, perl = TRUE))
    hora_exec <- regmatches(name, regexpr("[[:digit:]]{2}(?=.parquet$)", name, perl = TRUE))

    datahora_execucao <- paste0(data_exec, " ", hora_exec, ":00:00")
    datahora_execucao <- as.POSIXlt(datahora_execucao, format = "%Y%m%d %H:%M:%S", tz = "UTC")
    datahora_execucao <- as.POSIXct(datahora_execucao)

    return(datahora_execucao)
}

read_cams_dt <- function(obj) {

    datahora_exec <- parse_name(obj)
    dt <- as.data.table(s3read_using(read_parquet, object = obj))
    dt[, datahora_execucao := datahora_exec]
    colnames(dt)[3:4] <- c("datahora_previsao", "temperatura")
    setcolorder(dt, c("datahora_execucao", "datahora_previsao", "temperatura"), after = "longitude")

    dt[, origem := "cams"]
    dt[, hora_execucao := hour(datahora_exec)]
    dt[, horizonte := as.numeric(datahora_previsao - datahora_execucao) / 3600]

    return(dt)
}

get_closest <- function(dt, lat, lon, area = "", N = 1) {
    grid <- unique(dt[, .(latitude, longitude)])
    coords <- closest_coords(lat, lon, grid, N)
    dt <- merge(dt, coords, by = c("latitude", "longitude"))

    dt[, longitude := NULL]
    dt[, latitude := NULL]
    dt[, codigo_area := area]

    return(dt)
}

closest_coords <- function(lat, lon, grid, N = 1) {
    dists <- sqrt((grid$lat - lat)^2 + (grid$lon - lon)^2)
    grid[head(order(dists), N)]
}

# --------------------------------------------------------------------------------------------------

bucket <- "s3://ons-dl-dev-gefswind"
prefix <- "cams/2m_temperature/"
arqs <- get_bucket(bucket, prefix, max = Inf)

out <- structure(vector("list", nrow(area_grid)), names = area_grid$area)

for (arq in arqs) {

    if (as.Date(parse_name(arq)) <= as.Date("2023-05-16")) next
    if (grepl("-12.parquet", arq$Key)) next

    cat("* ", arq$Key, "\n")
    d_cams <- read_cams_dt(arq)

    for (i in area_grid[, seq_len(.N)]) {
        lat <- area_grid[i, lat]
        lon <- area_grid[i, lon]
        area <- area_grid[i, area]

        d_area <- get_closest(d_cams, lat, lon, area)
        setcolorder(d_area, c("origem", "codigo_area", "datahora_execucao", "datahora_previsao",
                "temperatura", "hora_execucao", "horizonte"))
        out[[area]] <- rbind(out[[area]], d_area)
    }
}

outdir <- "./data/banco/temperatura_prevista"
nomes <- sapply(names(out), function(area) {
    arq <- paste0("temperatura_prevista-origem=cams-codigo_area=", area, ".parquet.gzip")
    arq <- file.path(outdir, arq)
    arrow::write_parquet(out[[area]], arq)
})
