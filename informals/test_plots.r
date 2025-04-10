library(ggplot2)

plot_importance <- function(importance,
    topn = 100, group = TRUE, feature_pattern = NULL, model = NULL) {

    if (!is.null(model)) importance <- lgb.importance(model)

    importance <- melt(importance, id.vars = "Feature")
    importance <- importance[, .(Feature, value = sort(value)), by = variable]

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

    ggplot(importance) +
        geom_col(aes(x = value, y = Feature)) +
        facet_wrap(~ variable, nrow = 1, scale = "free_y") +
        theme_bw()

}

#importance <- lgb.importance(model)

plot_importance(importance, feature_pattern = "_V[[:digit:]]_.+(\\.V1)$", group = FALSE, topn = 60)
plot_importance(importance, group = TRUE)
plot_importance(importance, feature_pattern = "_W[[:digit:]]_.+(\\.V1)$", group = FALSE, topn = 60)
