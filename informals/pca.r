library(plotly)
library(zoo)
library(shapeshiftr)
library(moments)
library(glmnet)

source("R/leitura.r")

JANELA <- "2021/2024"

carga <- get_carga_historica("RJ", JANELA)
carga <- carga[din_referenciautc >= as.POSIXct("2021-01-02 00:00:00", "America/SaoPaulo")]
carga <- carga[din_referenciautc < as.POSIXct("2025-01-01 00:00:00", "America/SaoPaulo")]
carga[, valor_norm := scale(val_cargaglobalcons)]

slices <- slice(carga, "valor_norm", "din_referenciautc",
    L = seq(-47, 0), start = 48, step = 48, threads = 12)

slices_dt <- as.data.table(slices)

## PCA 

reduz_dado <- function(slices, N) {
    mm <- scale(slices[, -1])
    pca <- prcomp(mm)

    reduzido <- with(pca, x[, 1:N, drop = FALSE] %*% t(rotation[, 1:N, drop = FALSE]))

    reduzido_rescale <- sapply(seq_len(48), function(i) {
        reduzido[, i] * attr(mm, "scaled:scale")[i] + attr(mm, "scaled:center")[i]
    })
    serie_smooth <- c(t(reduzido_rescale))

    out <- carga[, .(din_referenciautc)]
    out[[paste0("pca_", N)]] <- serie_smooth

    return(out)
}

dados <- lapply(seq_len(30), reduz_dado, slices = slices_dt)
dados <- lapply(dados, melt, id.vars = "din_referenciautc")
dados <- rbindlist(dados)

dplot <- carga[, .(din_referenciautc, valor_norm)]
dplot <- melt(dplot, id.vars = "din_referenciautc")
dplot <- rbind(dplot, dados)
plot_ly(dplot[din_referenciautc <= "2022-01-01"], x = ~din_referenciautc, y = ~value, color = ~variable) %>% add_lines()

dd <- dplot[variable %in% c("valor_norm", "pca_1")]
dd <- dd[, mean(value), by = .(yday(din_referenciautc), variable)]

dd <- dcast(dd, yday ~ variable, value.var = "V1")
with(dd, plot(valor_norm, pca_1))


mm <- scale(slices_dt[, -1])
pca <- prcomp(mm)
dd <- carga[, var(valor_norm), by = as.Date(din_referenciautc)]
setorder(dd, as.Date)
plot(pca$x[, 2], dd$V1)


layout(matrix(1:2))
Y <- carga[, max(valor_norm), by = as.Date(din_referenciautc)]$V1
X <- lapply(seq_len(48), function(i) {
    mm <- poly(scale(pca$x[, i]), degree = 1, raw = TRUE)
    colnames(mm) <- paste0("pca_", i, "_", seq_len(1))
    mm
})
X <- do.call(cbind, X)
X <- scale(X)

CV <- cv.glmnet(X, Y)
mod <- glmnet(X, Y, lambda = CV$lambda.min)
plot(predict(mod, newx = X), Y, main = "48 pcas")
coef(mod)

Y <- carga[, max(valor_norm), by = as.Date(din_referenciautc)]$V1
X <- lapply(seq_len(1), function(i) {
    mm <- poly(scale(pca$x[, i]), degree = 1, raw = TRUE)
    colnames(mm) <- paste0("pca_", i, "_", seq_len(1))
    mm
})
X <- do.call(cbind, X)
X <- scale(X)

#CV <- cv.glmnet(X, Y)
mod <- lm(Y ~ X)
plot(predict(mod, newx = X), Y, main = "1 pcas")
coef(mod)