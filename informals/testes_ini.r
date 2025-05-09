#library(modprev)
library(lightgbm)
#library(shapeshiftr)
library(data.table)
library(zoo)
library(ggplot2)

source("R/utils.r")
source("R/leitura.r")
source("R/build_regressores.r")
source("R/cross_validation_utils.r")

options(arrow.unsafe_metadata = TRUE)

carga_obs <- get_carga_observada("SECO", "2020/2024")
temp_obs  <- get_temperatura_observada("SECO", "2020/2024")
temp_prev <- get_temperatura_prevista("SECO", "2020/2024")
feriados  <- get_feriados("SECO")

#reg <- build_regs_quant_singleshot(carga_obs, temp_obs, temp_prev)
#arrow::write_parquet(reg, "data/teste_SECO.parquet.gzip")

reg <- arrow::read_parquet("data/teste_SECO.parquet.gzip")
reg <- cbind(index = reg$index, as.data.table(reg[, lapply(.SD, scale), .SDcols = -1]))

reg <- add_regs_quali(reg, feriados)
reg <- add_regs_quali(reg, NULL, timefeatures = "data.table::month")

data <- merge(carga_obs[, -1], reg, by.x = "datahora", by.y = "index")

scale_carga <- data[, c(mean(carga), sd(carga))]
data[, carga := (carga - scale_carga[1]) / scale_carga[2]]

lm_carga <- lm(carga ~ datahora, data)
data[, carga := residuals(lm_carga)]

data_train <- data[
    (datahora < as.POSIXct("2024-10-01 00:00:00", "GMT"))]
data_test  <- data[
    datahora >= as.POSIXct("2024-10-01 00:00:00", "GMT") &
    datahora <  as.POSIXct("2024-11-01 00:00:00", "GMT")
]

pesos <- ifelse(nhour(data_train$datahora) %between% c(18.5, 21.5), 48 / 7, 1)
pesos <- pesos / sum(pesos)

dataset_train <- lgb.Dataset(
    data.matrix(data_train[, -(1:2)]),
    label = data_train$carga,
    weight = pesos,
    params = list(
        linear_tree = FALSE
    )
)

dataset_test <- lgb.Dataset(
    data.matrix(data_test[, -(1:2)]),
    label = data_test$carga,
    params = list(
        linear_tree = FALSE
    )
)

#index_date <- as.Date(data_train$datahora)
#folds <- sample_stratified_folds(index_date, nfolds = 10, split_in = 1)
#

folds2 <- rep(rep(seq_len(4), each = 7 * 48), length.out = nrow(dataset_train))
folds2 <- split(seq_len(nrow(dataset_train)), folds2)
  
fold_dates <- lapply(folds2, function(f) data_train[f, datahora])
fold_dates <- lapply(fold_dates, as.Date)
fold_dates <- lapply(fold_dates, unique)
fold_dates <- lapply(fold_dates, function(f) f[order(month(f))])

lapply(fold_dates, function(f) table(month(f)))

CV_PARAMS <- expand.grid(
    min_data_in_leaf = c(25, 50, 100),
    num_leaves = 2^c(4, 5)
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
        stratified = FALSE,
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

    pred <- predict(model, newdata = data.matrix(data_test[, -(1:2)]))
    pred <- pred + predict(lm_carga, newdata = data_test)
    pred <- pred * scale_carga[2] + scale_carga[1]

    dpred <- cbind(data_test[, .(datahora)], carga = pred)

    d_erro <- merge(dpred, carga_obs, by = "datahora")
    d_erro[, hora := nhour(datahora)]
    CV_PARAMS$valid_mape[i] <- d_erro[, mean(abs(carga.y - carga.x) / carga.y)]
    CV_PARAMS$valid_mape_ponta[i] <- d_erro[hora %between% c(18.5, 21.5), mean(abs(carga.y - carga.x) / carga.y)]
    CV_PARAMS$valid_rmse[i] <- d_erro[, sqrt(mean((carga.y - carga.x)^2))]
    CV_PARAMS$valid_bias2[i] <- d_erro[, mean(carga.y - carga.x)^2]
    CV_PARAMS$valid_var[i]   <- d_erro[, var(carga.y - carga.x)]

    rm("mod", "model")
    gc()
}

model <- lgb.train(
    data = dataset_train,
    nrounds = 5000L,
    params = list(
        min_data_in_leaf = CV_PARAMS$min_data_in_leaf[i],
        num_leaves = CV_PARAMS$num_leaves[i]
    ),
    valids = list("valid1" = dataset_test),
    verbose = 1
)

I <- 5000
I <- CV_PARAMS$best_iter[i]

pred <- predict(model, newdata = data.matrix(data_test[, -(1:2)]), num_iteration = 3300)
pred <- pred + predict(lm_carga, newdata = data_test)
pred <- pred * scale_carga[2] + scale_carga[1]

dpred <- cbind(data_test[, .(datahora)], carga = pred)

d_erro <- merge(dpred, carga_obs, by = "datahora")
d_erro[, mean(abs(carga.y - carga.x) / carga.y)]

d_erro[, hora := nhour(datahora)]
d_erro[hora %between% c(18.5, 21.5), mean(abs(carga.y - carga.x) / carga.y)]

ggplot() +
    geom_line(data = carga_obs[datahora %in% dpred$datahora],
        aes(datahora, carga, colour = "observado")) +
    geom_line(data = dpred,
        aes(datahora, carga, colour = "previsto"))
