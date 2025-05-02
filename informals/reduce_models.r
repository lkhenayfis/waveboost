source("R/waveboost.r")

models <- list.files("out/models", full.names = TRUE)

for (model in models) {
    obj <- readRDS(model)
    obj$trend$model <- NULL
    obj$trend$fitted.values <- NULL
    obj$trend$residuals <- NULL
    obj$trend$effects <- NULL
    obj$trend$qr$qr <- NULL
    save_model(obj, model)
}
