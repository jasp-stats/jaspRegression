test_that("Bayesian regression producers retain only serializable drawing inputs", {
  ns <- asNamespace("jaspRegression")
  output <- function() {
    plot <- new.env(parent = emptyenv())
    plot$setError <- function(message) stop(message)
    plot
  }
  post <- list(coef = 1:3, conf95 = matrix(c(-1, 0, 1, 1, 2, 3, 0, 1, 2), nrow = 3),
               loopIdx = 1:3, coefficients = c("Intercept", "x", "z"))
  model <- list(postprobs = rep(1 / 30, 30), n.models = 30,
                size = rep(1:3, 10), logmarg = seq(-10, -5, length.out = 30),
                probne0 = c(1, .4, .7), namesx = c("Intercept", "x", "z"),
                priorprobsPredictor = c(1, .5, .5), n.vars = 3)
  set.seed(333)
  fit <- BAS::bas.lm(mpg ~ wt + hp, data = mtcars, prior = "ZS-null", modelprior = BAS::uniform())
  cases <- list(
    FillPlotPosteriorSummary = list(post, list(posteriorSummaryPlotWithoutIntercept = FALSE)),
    FillmodelProbabilitiesPlot = list(model),
    FillmodelComplexityPlot = list(model),
    FillinclusionProbabilitiesPlot = list(model),
    FillresidualsVsFittedPlot = list(fit),
    FillPlotQQ = list(fit)
  )
  for (prefix in c(".basreg", ".bayesianLogisticReg")) {
    for (suffix in names(cases)) {
      plot <- output()
      do.call(get(paste0(prefix, suffix), ns), c(list(plot), cases[[suffix]]))
      expect_s3_class(plot$plotObject, "jaspPlotRecipe")
      recipe <- unserialize(serialize(plot$plotObject, NULL))
      rng <- .Random.seed
      first <- jaspGraphs::materializeJaspPlotRecipe(recipe)
      second <- jaspGraphs::materializeJaspPlotRecipe(recipe)
      expect_s3_class(first, "ggplot")
      expect_identical(.Random.seed, rng)
      expect_equal(ggplot2::ggplot_build(first)$data, ggplot2::ggplot_build(second)$data)
      if (suffix == "FillPlotPosteriorSummary")
        expect_identical(first$scales$get_scales("y")$name, expression(beta))
    }
  }
})

test_that("linear residual insertion uses recipes without retaining fitted models", {
  ns <- asNamespace("jaspRegression")
  fit <- lm(mpg ~ wt, data = mtcars)
  cases <- list(
    list(helper = ".linregPlotResiduals", args = list(xVar = fitted(fit), res = residuals(fit), xlab = "Fitted")),
    list(helper = ".linregPlotResiduals", args = list(xVar = factor(rep(c("a", "b"), 16)), res = residuals(fit), xlab = "Group")),
    list(helper = ".linregPlotResidualsHistogram", args = list(res = residuals(fit), resName = "Residuals"))
  )
  for (case in cases) {
    plot <- new.env()
    plot$setError <- function(message) stop(message)
    do.call(get(".linregInsertPlot", ns), c(list(plot, get(case$helper, ns)), case$args))
    expect_s3_class(plot$plotObject, "jaspPlotRecipe")
    recipe <- unserialize(serialize(plot$plotObject, NULL))
    materialized <- jaspGraphs::materializeJaspPlotRecipe(recipe)
    expected <- do.call(get(case$helper, ns), case$args)
    expect_equal(ggplot2::ggplot_build(materialized)$data, ggplot2::ggplot_build(expected)$data)
    expect_identical(plot$status, "complete")
  }
})

test_that("shared Q-Q preparation freezes residuals before drawing", {
  ns <- asNamespace("jaspRegression")
  fit <- lm(mpg ~ wt, data = mtcars)
  opts <- list(qqPlotCi = TRUE, qqPlotCiLevel = .95)
  recipe <- get(".glmFillPlotResQQ", ns)("deviance", fit, opts)
  expect_s3_class(recipe, "jaspPlotRecipe")
  recipe <- unserialize(serialize(recipe, NULL))
  expect_type(recipe$args$residuals, "double")
  expected <- jaspGraphs::plotQQnorm(recipe$args$residuals, ablineColor = "darkred",
                                   ablineOrigin = TRUE, identicalAxes = TRUE, ciLevel = .95)
  expect_equal(ggplot2::ggplot_build(jaspGraphs::materializeJaspPlotRecipe(recipe))$data,
               ggplot2::ggplot_build(expected)$data)
})

test_that("heatmap recipes store prepared coefficient labels and factor ordering", {
  ns <- asNamespace("jaspRegression")
  options <- list(variables = c("Predictor", "Outcome"), significanceFlagged = TRUE)
  results <- list(list(vars = options$variables,
                       res = list(pearson = list(estimate = .4567, p.value = .009))))
  recipe <- get(".corrPlotHeatmap", ns)("pearson", options, results)
  expect_s3_class(recipe, "jaspPlotRecipe")
  recipe <- unserialize(serialize(recipe, NULL))
  expect_identical(levels(recipe$args$data$var1), options$variables)
  expect_identical(levels(recipe$args$data$var2), rev(options$variables))
  expect_true(all(recipe$args$data$label[!is.na(recipe$args$data$cor)] == "0.457**"))
  expect_s3_class(jaspGraphs::materializeJaspPlotRecipe(recipe), "ggplot")
})
