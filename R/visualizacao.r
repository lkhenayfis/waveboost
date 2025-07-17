
#' Plot De Importancia Dos Regressores
#' 
#' Wrapper com utilitarios facilitadores para visualizacao de importancia dos regressores
#' 
#' @param importance um objeto retornado por `lightgbm::lgb.importance()`
#' @param topn inteiro indicando maximo numero de regressores para plotar
#' @param group booleano indicando se agrupamento de regressores deve ser feito
#' @param feature_pattern string indicando padrao regex para filtrar regressores
#' @param order_by um de `c("Gain", "Cover", "Frequency")` indicando por qual ordenar
#' @param model se `importance` nao for passado, pode ser passado um modelo ajustado para calcular
#'     a importancia
#' 
#' @return objeto `ggplot` com o grafico de importancia

plot_importance <- function(importance,
    topn = 100, group = TRUE, feature_pattern = NULL, order_by = "Gain", model = NULL) {

    if (!is.null(model)) importance <- lgb.importance(model)

    setorderv(importance, cols = order_by, order = -1)
    importance <- melt(importance, id.vars = "Feature")

    if (!is.null(feature_pattern)) {
        importance <- importance[grep(feature_pattern, Feature)]
    }

    if (group) {
        importance[, Feature := sub("(_[[:digit:]]+(\\.V1)?)$", "", Feature)]
        importance <- importance[, .(value = sum(value)), by = .(Feature, variable)]
    }

    importance <- split(importance, importance$variable)
    importance <- lapply(importance, function(d) d[order(value, decreasing = TRUE)])
    importance <- lapply(importance, function(d) head(d, topn))
    importance <- rbindlist(importance)
    importance[, Feature := factor(Feature, rev(unique(Feature)))]

    ggplot(importance) +
        geom_col(aes(x = value, y = Feature)) +
        facet_wrap(~ variable, nrow = 1, scale = "free_y") +
        theme_bw()
}
