context("Bayesian Logistic Regression")

# Currently does not implement anything
#options <- jaspTools::analysisOptions("~/Downloads/santas_log.jasp")

#jaspTools::runAnalysis("RegressionLogisticBayesian", "tests/testthat/santas_log.csv", options, makeTests = TRUE)


options <- jaspTools::analysisOptions("RegressionLogisticBayesian")
options$.meta <- list(covariates = list(shouldEncode = TRUE), dependent = list(
  shouldEncode = TRUE), factors = list(shouldEncode = TRUE),
  modelTerms = list(shouldEncode = TRUE), residualSdsSavedToDataColumn = list(
    shouldEncode = TRUE), residualsSavedToDataColumn = list(
      shouldEncode = TRUE), weights = list(shouldEncode = TRUE))
options$covariates <- "quantity"
options$dependent <- "delivered"
options$factors <- "treat"
options$modelPrior <- "betaBinomial"
options$modelTerms <- list(list(components = "treat", isNuisance = FALSE), list(components = "quantity",
                                                                                isNuisance = FALSE))
options$posteriorSummaryPlot <- TRUE
options$posteriorSummaryTable <- TRUE
options$priorRegressionCoefficients <- "cch"
options$residualSdsSavedToDataColumn <- ""
options$residualsSavedToDataColumn <- ""
set.seed(1)
results <- jaspTools::runAnalysis("RegressionLogisticBayesian", "santas_log.csv", options)


test_that("Model Comparison - delivered table results match", {
  table <- results[["results"]][["bayesianLogisticReg"]][["collection"]][["bayesianLogisticReg_modelComparisonTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 24815169383.8636, "treat + quantity", 0.223989473430303, 0.999999999919404,
                                      0.333333333333333, 1.61146603924989e-10, 4.02866509812463e-10,
                                      "treat", 0.129912187559307, 8.05733019560006e-11, 0.166666666666667,
                                      4.5173021156918e-14, 1.12932552883196e-13, "quantity", 0.0983391518892499,
                                      2.25865105766386e-14, 0.166666666666667, 7.08231841392598e-24,
                                      1.41646368267103e-23, "Null model", 0, 7.08231841335515e-24,
                                      0.333333333333333))
})

test_that("Posterior Coefficients with 95% Credible Interval plot matches", {
  plotName <- results[["results"]][["bayesianLogisticReg"]][["collection"]][["bayesianLogisticReg_postSumContainer"]][["collection"]][["bayesianLogisticReg_postSumContainer_postSumPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "posterior-coefficients-with-95-credible-interval")
})

test_that("Posterior Summaries of Coefficients table results match", {
  table <- results[["results"]][["bayesianLogisticReg"]][["collection"]][["bayesianLogisticReg_postSumContainer"]][["collection"]][["bayesianLogisticReg_postSumContainer_postSumTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, "Intercept", 2.52390178513271, 3.20607964259812, 0, 0, 1, 1,
                                      0.34424219810404, 3.87295330988576, 44274213862885.1, "treat (1)",
                                      -2.44114247242923, -1.92270259080336, 2.25375273998907e-14,
                                      0.5, 0.999999999999977, 0.5, 0.261616354607274, -1.41589355089097,
                                      12411058944.3811, "quantity", -0.893625282446246, -0.68243241619767,
                                      8.05733257891461e-11, 0.5, 0.999999999919427, 0.5, 0.106572641921755,
                                      -0.475977516565109))
})
