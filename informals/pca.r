library(plotly)
library(ggplot2)

source("R/waveboost.r")

carga <- get_carga_observada("SP", "2022/2024")
temp_obs  <- get_temperatura_observada("SP", "2022/2024")
temp_prev <- get_temperatura_prevista("SP", "2022/2024")
feriados <- get_feriados("SP")

lm_carga <- lm(cargaglobalcons ~ datahora, carga)
carga[, cargaglobalcons_detrend := residuals(lm_carga)]

JANELA <- c("2024-11-01", "2024-12-01")

carga_treino <- carga[datahora < JANELA[1]]
temp_obs_treino <- temp_obs[datahora < JANELA[1]]
temp_prev_treino <- temp_prev[datahora_execucao < JANELA[1]]

carga_teste <- carga[(datahora >= JANELA[1]) & (datahora < JANELA[2])]
temp_obs_teste <- temp_obs[(datahora >= JANELA[1]) & (datahora < JANELA[2])]
temp_prev_teste <- temp_prev[(datahora_execucao >= JANELA[1]) & (datahora_execucao < JANELA[2])]

# AUXILIARES ---------------------------------------------------------------------------------------

#' Wrapper Mais Completo De `prcomp`
#' 
#' Realiza decomposicao em componentes principais guardando informacoes necessarias para inversao
#' 
#' @param x `data.table` com dados nos quais aplicar PCA; cada linha e tratada como uma amostra
#' 
#' @return objeto da classe `prcomp`. Veja `?prcomp` para mais detalhes

pca2 <- function(x, ...) {
    x <- scale2(x)
    pca <- prcomp(data.matrix(x), center = TRUE, scale = TRUE)

    new_pca2(pca, x)
}

new_pca2 <- function(x, ref) {
    out <- list(x, hollow_reference(ref))
    class(out) <- "pca2"

    return(out)
}

#' Reverte Transformacao Com `N` Componentes
#' 
#' Transformacao inversa de uma PCA ja realizada utilizando apenas `N` componentes principais
#' 
#' @param pca saida de `pca2`
#' @param N inteiro indicando numero de componentes a utilizar; -1 usa todas
#' 
#' @return `data.table` de dados reconstruidos

inv_pca2 <- function(pca, Z = NULL, N = -1) {
    N <- ifelse(N == -1, ncol(pca[[1]]$rotation), N)

    M <- pca[[1]]$rotation[, seq_len(N)]
    if (is.null(Z)) Z <- pca[[1]]$x[, seq_len(N)]

    if (N == -1) N <- ncol(M)

    x <- Z %*% t(M)
    x <- as.data.table(x)
    x <- rescale2(x, pca[[2]])

    return(x)
}

# PESOS --------------------------------------------------------------------------------------------

xpca <- dcast(carga, as.Date(datahora) ~ nhour(datahora), value.var = "cargaglobalcons_detrend")

pca <- pca2(xpca[, -1])
signif <- pca[[1]]$sdev

pesos_sd  <- signif / sum(signif)
pesos_sd1 <- sqrt(signif) / sum(sqrt(signif))
pesos_var <- signif^2 / sum(signif^2)

plot(pesos_var, type = "o", panel.first = grid(col = "grey85"))
lines(pesos_sd, type = "o", col = 4)
lines(pesos_sd1, type = "o", col = 3)

plot(cumsum(pesos_var), type = "o", panel.first = grid(col = "grey85"), ylim = c(0, 1))
lines(cumsum(pesos_sd), type = "o", col = 4)
lines(cumsum(pesos_sd1), type = "o", col = 3)
abline(h = .95, col = "grey70", lty = 2)

# ANALISE INICIAL PCA ------------------------------------------------------------------------------

reconstruidos <- lapply(seq_len(48), function(n) {
    d <- inv_pca2(pca, n)
    d <- c(t(data.matrix(d)))
    d <- data.table(d)
    names(d) <- paste0(n, "_componentes")
    d
})
reconstruidos <- do.call(cbind, reconstruidos)

dplot <- cbind(carga_treino[, .(datahora, cargaglobalcons)], reconstruidos)
dplot <- melt(dplot, id.vars = "datahora")

p <- plot_ly(dplot, x = ~datahora, y = ~value, color = ~variable) %>% add_lines()
print(p)

# MSE POR RECONSTRUCAO -----------------------------------------------------------------------------

erro0 <- sqrt(mean((mean(data.matrix(xpca[, -1])) - data.matrix(xpca[, -1]))^2))
erros <- sapply(seq_len(48), function(n) {
    d <- inv_pca2(pca, N = n)
    mse <- data.matrix(d) - data.matrix(xpca[, -1])
    sqrt(mean(mse^2))
})
erros <- c(erro0, erros)
erros <- erros / sum(abs(erros))

diff_erros <- -diff(c(erro0, erros))
diff_erros <- diff_erros / sum(diff_erros)

jpeg("comparacao_pesagem.jpeg", width = 800, height = 600, res = 100)
plot(pesos_var, type = "o", panel.first = grid(col = "grey85"), ylim = c(0, 1), pch = 16,
    xlab = "Indice de componente principal", ylab = "Peso acumulado")
lines(pesos_sd, type = "o", col = 4, pch = 16)
lines(pesos_sd1, type = "o", col = 3, pch = 16)
lines(erros, type = "o", col = 2, pch = 16)
abline(h = .95, col = "grey70", lty = 2)
legend("bottomright", inset = .02,
    title = "Tipo de pesagem", legend = c("var", "sd", "sd^1/2", "rmse"), pch = 16, col = c(1, 4, 3, 2))
dev.off()

plot(pesos_var, type = "o", panel.first = grid(col = "grey85"), ylim = c(0, .3), pch = 16,
    xlab = "Indice de componente principal", ylab = "Peso acumulado")
lines(pesos_sd, type = "o", col = 4, pch = 16)
lines(pesos_sd1, type = "o", col = 3, pch = 16)
lines(erros, type = "o", col = 2, pch = 16)
lines(diff_erros, type = "o", col = 2, pch = 16, lty = 2)
abline(h = .95, col = "grey70", lty = 2)
legend("bottomright", inset = .02,
    title = "Tipo de pesagem", legend = c("var", "sd", "sd^1/2", "rmse"), pch = 16, col = c(1, 4, 3, 2))

# TESTANDO MODELOS ---------------------------------------------------------------------------------

xpca <- dcast(carga, as.Date(datahora) ~ nhour(datahora), value.var = "cargaglobalcons_detrend")
pca_0 <- pca2(xpca[, -1])

#scale_x <- scale(pca_0[[1]]$x)
scale_x <- pca_0[[1]]$x

pca <- data.table(
    data = as.Date(carga$datahora),
    componente = seq(1, 48),
    valor = c(t(scale_x)))

onehots <- as_onehot(pca$componente, seq_len(48))
names(onehots) <- paste0("pca", names(onehots))

pca <- cbind(pca, onehots)

pipes <- list(
    list(
        on =  list("carga"),
        transforms =  list(
            list(
                fun =  "gen_dwt_builder_lag",
                var =  "cargaglobalcons_detrend", hora_execucao = "07:30:00", L = 64
            )
        )
    ),
    list(
        on =  list("carga", "feriados"),
        transforms =  list(
            list(
                fun =  "gen_merger_feriado"
            )
        )
    ),
    list(
        on = list("temp_obs", "temp_prev"),
        transforms = list(
            list(
                fun = "gen_dwt_builder_laglead",
                var_x = "temperatura", var_y = "temperatura",
                hora_execucao = "07:30:00", L = 64, roll = FALSE
            )
        )
    ),
    list(
        on = list("carga"),
        transforms = list(
            list(
                fun = "gen_timefeature_adder",
                timefeatures = list(month = "data.table::month", wday = "data.table::wday"), onehot = TRUE
            )
        )
    )
)
pipes <- parse_pipes(pipes)

DATA <- eval_pipes(pipes)
DATA[, index := as.Date(index)]
DATA <- unique(DATA)



regs <- merge(DATA, pca, by.x = "index", by.y = "data")
regs <- scale2(regs)
regs$componente <- NULL

regs_train <- regs[
    (index < as.Date("2024-11-01"))]
regs_test  <- regs[
    (index >= as.Date("2024-11-01")) & (index <  as.Date("2024-12-01"))
]

pesos <- rep(pca_0[[1]]$sdev, length.out = nrow(regs_train))
pesos <- rep(1, length.out = nrow(regs_train))
pesos <- pesos / sum(pesos)

dataset_train <- lgb.Dataset(
    data.matrix(regs_train[, .SD, .SDcols = -c("index", "valor")]),
    label = regs_train$valor#,
    #weight = pesos
)

dataset_test <- lgb.Dataset(
    data.matrix(regs_test[, .SD, .SDcols = -c("index", "valor")]),
    label = regs_test$valor
)


folds2 <- rep(rep(seq_len(4), each = 7 * 48), length.out = nrow(dataset_train))
folds2 <- split(seq_len(nrow(dataset_train)), folds2)

CV_PARAMS <- expand.grid(
    min_data_in_leaf = c(25, 50, 100),
    num_leaves = c(8, 16, 32)
)

#CV_PARAMS <- lapply(c(5, 6), function(i) {
#    expand.grid(
#        min_data_in_leaf = c(25, 50),
#        max_leaves = 2^i,
#        max_depth = seq(i, by = 2 * floor(i / 2), length.out = 3)
#    )
#})
#CV_PARAMS <- do.call(rbind, CV_PARAMS)

CV_PARAMS$best_score  <- NA_real_
CV_PARAMS$best_iter   <- NA_integer_
CV_PARAMS$valid_mape  <- NA_real_
CV_PARAMS$valid_rmse  <- NA_real_
CV_PARAMS$valid_bias2 <- NA_real_
CV_PARAMS$valid_var   <- NA_real_
CV_PARAMS$valid_mape_ponta <- NA_real_

for (i in seq_len(nrow(CV_PARAMS))) {
    cat("========================", "\n")
    cat("CONJUNTO DE PARAMETROS:", i, "[", paste(CV_PARAMS[i, 1:2], collapse = "-"), "]", "\n")
    cat("========================", "\n\n")

    mod <- lgb.cv(
        data = dataset_train,
        nrounds = 5000L,
        #nfold = 5L,
        folds = folds2,
        params = list(
            min_data_in_leaf = CV_PARAMS[i, 1],
            max_leaves = CV_PARAMS[i, 2],
            feature_pre_filter = FALSE
        ),
        early_stopping_rounds = 50L,
        verbose = 1,
        eval_freq = 100L
    )

    CV_PARAMS$best_score[i] <- mod$best_score
    CV_PARAMS$best_iter[i]  <- mod$best_iter

    model <- lgb.train(
        data = dataset_train,
        nrounds = CV_PARAMS$best_iter[i],
        params = list(
            min_data_in_leaf = CV_PARAMS[i, 1],
            num_leaves = CV_PARAMS[i, 2]
        ),
        verbose = 1
    )

    pred <- predict(model, newdata = data.matrix(regs_test[, .SD, .SDcols = -c("index", "valor")]))
    #pred <- pred * attr(scale_x, "scaled:scale") - attr(scale_x, "scaled:center")
    pred <- c(t(inv_pca2(pca_0, matrix(pred, ncol = 48, byrow = TRUE))))
    pred <- pred + predict(lm_carga, newdata = carga_teste)

    dpred <- cbind(carga_teste[, .(datahora)], carga = pred)

    d_erro <- merge(dpred, carga[, .(datahora, carga = cargaglobalcons)], by = "datahora")
    d_erro[, hora := nhour(datahora)]
    CV_PARAMS$valid_mape[i] <- d_erro[, mean(abs(carga.y - carga.x) / carga.y)]
    CV_PARAMS$valid_rmse[i] <- d_erro[, sqrt(mean((carga.y - carga.x)^2))]
    CV_PARAMS$valid_var[i]  <- d_erro[, var(carga.y - carga.x)]
    CV_PARAMS$valid_bias2[i] <- d_erro[, mean(carga.y - carga.x)^2]
    CV_PARAMS$valid_mape_ponta[i] <- d_erro[
        (hora - 3) %between% c(18.5, 21.5),
        mean(abs(carga.y - carga.x) / carga.y)
    ]

    rm("mod", "model")
    gc()
}

which_best <- which.min(CV_PARAMS$best_score)

model <- lgb.train(
    data = dataset_train,
    nrounds = CV_PARAMS$best_iter[which_best],
    params = list(
        min_data_in_leaf = CV_PARAMS$min_data_in_leaf[which_best],
        num_leaves = CV_PARAMS$num_leaves[which_best]
    ),
    verbose = 1
)

pred <- predict(model, newdata = data.matrix(regs_test[, .SD, .SDcols = -c("index", "valor")]))
#pred <- pred * attr(scale_x, "scaled:scale") - attr(scale_x, "scaled:center")
pred <- c(t(inv_pca2(pca_0, matrix(pred, ncol = 48, byrow = TRUE))))
pred <- pred + predict(lm_carga, newdata = carga_teste)

dpred <- cbind(carga_teste[, .(datahora)], carga = pred)

d_erro <- merge(dpred, carga[, .(datahora, carga = cargaglobalcons)], by = "datahora",
    suffixes = c("_prev", ""))
d_erro[, mean(abs(carga - carga_prev) / carga)]

d_erro[, hora := nhour(datahora)]
d_erro[(hora - 3) %between% c(18.5, 21.5), mean(abs(carga - carga_prev) / carga)]

dplot <- melt(d_erro, id.vars = c("datahora"), measure.vars = c("carga_prev", "carga"))

ggplot(dplot, aes(datahora, value, color = variable)) +
    geom_line() +
    facet_wrap(~ as.Date(datahora), scales = "free_x")

ii <- lgb.importance(model)
plot_importance(ii)
plot_importance(ii, group = FALSE, topn = 50)
plot_importance(ii,
    feature_pattern = "pca", group = FALSE, topn = 128)
