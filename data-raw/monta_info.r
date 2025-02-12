library(LoadServices)
library(data.table)

dt1 <- as.data.table(GetAreasCarga()$results)[, .SD, .SDcols = 2:3]
colnames(dt1) <- c("codigo_area", "nome_area")
dt1[, nome_area := iconv(nome_area, "", "ASCII//TRANSLIT")]
dt1 <- dt1[!(codigo_area %in% c("TO", "TOCO"))]
dt1 <- dt1[!grepl("Perdas", nome_area)]
setorder(dt1, nome_area)

dt2 <- as.data.table(SerieSPCA())[, .SD, .SDcols = 1:2]
colnames(dt2) <- c("id_serie", "nome_area")
dt2[, nome_area := iconv(nome_area, "", "ASCII//TRANSLIT")]
dt2[, nome_area := sub("^[[:digit:]]{2}\\-", "", nome_area)]
dt2[, nome_area := sub("( \\(.*\\))$", "", nome_area)]
dt2 <- dt2[!(nome_area %in% "Parana")]
setorder(dt2, nome_area)

info_areas <- merge(dt1, dt2, by = "nome_area")

missing_dt1 <- dt1[!(dt1$nome_area %in% info_areas$nome_area)]
missing_dt2 <- dt2[!(dt2$nome_area %in% info_areas$nome_area)]

reorder <- c(1, 3, 2, 4, 5, 7, 6, 12)

missing_info <- copy(missing_dt1)
missing_info[, id_serie := missing_dt2[reorder, id_serie]]

info_areas <- rbind(info_areas, missing_info)

fwrite(info_areas, "data/info_areas.csv")
