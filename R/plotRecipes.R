# Pure drawing boundaries shared by Bayesian linear and logistic regression.
# Validate drawing now to retain existing analysis error handling; store only the recipe.
.regressionPlotRecipe <- function(fun, args) {
  recipe <- jaspGraphs::createJaspPlotRecipe(fun = fun, args = args)
  jaspGraphs::materializeJaspPlotRecipe(recipe)
  recipe
}

.regressionDrawSummary <- function(df, confInt) {
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(range(c(confInt)))
  g <- ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = x, y = y, ymin = lower, ymax = upper)) +
    ggplot2::geom_point(size = 4) +
    ggplot2::geom_errorbar(, width = 0.2) +
    ggplot2::scale_x_discrete(name = "") +
    ggplot2::scale_y_continuous(name = expression(beta), breaks = yBreaks, limits = range(yBreaks))
  jaspGraphs::themeJasp(g) +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(angle = 0, vjust = .5, size = 20)
    )
}

.regressionDrawBMAResiduals <- function(dfPoints) {
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(dfPoints[["x"]], 3)
  g <- jaspGraphs::drawAxis()
  g <- g + ggplot2::geom_hline(yintercept = 0, linetype = 2, col = "gray")
  g <- jaspGraphs::drawPoints(g, dat = dfPoints, size = 2, alpha = .85)
  g <- jaspGraphs::drawSmooth(g, dat = dfPoints, color = "red", alpha = .7) +
    ggplot2::ylab("Residuals") +
    ggplot2::scale_x_continuous(name = gettext("Predictions under BMA"), breaks = xBreaks, limits = range(xBreaks))
  jaspGraphs::themeJasp(g)
}

.regressionDrawModelProbabilities <- function(dfPoints, nModels) {
  xBreaks <- round(seq(1, nModels, length.out = min(5, nModels)))
  g <- jaspGraphs::drawSmooth(dat = dfPoints, color = "red", alpha = .7)
  g <- jaspGraphs::drawPoints(g, dat = dfPoints, size = 4) +
    ggplot2::scale_y_continuous(name = gettext("Cumulative Probability"), limits = 0:1) +
    ggplot2::scale_x_continuous(name = gettext("Model Search Order"), breaks = xBreaks)
  jaspGraphs::themeJasp(g)
}

.regressionDrawModelComplexity <- function(dfPoints, dim, logmarg) {
  # gonna assume here that dim (the number of parameters) is always an integer
  xBreaks <- unique(round(pretty(dim)))
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(range(logmarg))
  g <- jaspGraphs::drawPoints(dat = dfPoints, size = 4) +
    ggplot2::scale_y_continuous(name = gettext("Log(P(data|M))"),  breaks = yBreaks, limits = range(yBreaks)) +
    ggplot2::scale_x_continuous(name = gettext("Model Dimension"), breaks = xBreaks)
  jaspGraphs::themeJasp(g)
}

.regressionDrawInclusion <- function(dfBar, dfLine, width, base, priorProb, probne0) {
  yLimits <- c(0, base * ceiling(max(c(priorProb, probne0)) / base))
  yBreaks <- seq(yLimits[1], yLimits[2], length.out = 5)

  g <- ggplot2::ggplot(data = dfBar, mapping = ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_bar(width = width, stat = "identity", fill = "gray80", show.legend = FALSE)
  g <- jaspGraphs::drawLines(g, dat = dfLine,
                             mapping = ggplot2::aes(x = x, y = y, group = g, linetype = g0), show.legend = TRUE) +
    ggplot2::scale_y_continuous(gettext("Marginal Inclusion Probability"), breaks = yBreaks, limits = yLimits) +
    ggplot2::xlab("") +
    ggplot2::scale_linetype_manual(name = "", values = 2, labels = gettext("Prior\nInclusion\nProbabilities"))

  jaspGraphs::themeJasp(g, horizontal = TRUE, legend.position = "right") +
    ggplot2::theme(
      legend.title = ggplot2::element_text(size = .8*jaspGraphs::graphOptions("fontsize"))
    )
}
