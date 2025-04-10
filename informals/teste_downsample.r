library(data.table)

source("R/leitura.r")
source("R/utils.r")

x1 <- sample(seq(10), 6)
upsample(x1, 2, "linear", FALSE)
upsample(x1, 2, "linear", TRUE)

x2 <- as.POSIXct(c("2020-01-01 00:00", "2020-01-01 03:00", "2020-01-01 10:00"))
upsample(x2, 2, "linear", TRUE)

dd <- data.frame(datahora = x2, valor = x1[1:3])
dd2 <- upsample(dd, 3, "linear", time_col = "datahora", value_col = "valor")

dd <- data.table(A = "teste", B = Sys.Date(), datahora = x2, valor = x1[1:3])
dd2 <- upsample(dd, 3, "linear", time_col = "datahora", value_col = "valor", by = c("A", "B"))


dd <- head(get_temperatura_prevista("SP", "2024-01-01"), 12)
dd2 <- upsample(dd, 2, "linear", time_col = "datahora_previsao", value_col = "temperatura",
    by = c("codigo_area", "datahora_execucao"))
