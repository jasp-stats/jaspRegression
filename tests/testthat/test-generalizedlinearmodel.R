context("GeneralizedLinearModel")

getOptions <- function(analysisName) {
  options <- jaspTools::analysisOptions(analysisName)
  options <- addCommonQmlOptions(options)
  return(options)
}

addCommonQmlOptions <- function(options) {
  # jaspTools doesn't recognize common QML elements so this function adds the defaults manually
  #root <- testthat::test_path(file.path("..", "..", "inst", "qml", "common"))
  root <- file.path("..", "..", "inst", "qml", "common")
  c(
    options,
    jaspTools:::readQML(file.path(root, "GlmInputComponent.qml")),
    jaspTools:::readQML(file.path(root, "GlmResidualAnalysisPlotsComponent.qml")),
    jaspTools:::readQML(file.path(root, "EmmComponent.qml"))
  )
}

#### Test the binomial distribution with different links ####
# Specifically, the model coefficients, based on the GLM book (Dunn and Smyth, 2018) (page 337)
test_that("Binomial regression results match", {
  options <- getOptions("GeneralizedLinearModel")
  options$covariates <- c("Hours")
  options$weights    <- "Turbines"
  options$dependent  <- "DV"
  options$modelTerms <- list(
    list(components="Hours", isNuisance=FALSE)
  )
  options$family     <- "binomial"
  options$coefficientCi <- TRUE
  options$coefficientCiLevel <- 0.95

  #test logit link
  options$link       <- "logit"
  results <- jaspTools::runAnalysis("GeneralizedLinearModel", "turbines.csv", options)
  table <- results[["results"]][["estimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table, list("(Intercept)", -3.924, 0.378, -10.381, .000, -4.704, -3.219,
                                             "Hours",       0.001,  0.000, 8.754,   .0001, 0.0008, 0.0012))

  #test probit
  options$link       <- "probit"
  results <- jaspTools::runAnalysis("GeneralizedLinearModel", "turbines.csv", options)
  table <- results[["results"]][["estimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table, list("(Intercept)", -2.276, 0.1974, -11.528, .000, -2.676, -1.901,
                                             "Hours",       0.0006, 0.000,  9.239,   .0001,0.0005,  0.0007))

  #test cloglog
  options$link       <- "cloglog"
  results <- jaspTools::runAnalysis("GeneralizedLinearModel", "turbines.csv", options)
  table <- results[["results"]][["estimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table, list("(Intercept)", -3.603, 0.3247, -11.096,  .000, -4.268, -3.003,
                                             "Hours",       0.0008, 0.0001,  8.992,   .000, 0.0006,  0.001))
})


#### test other aspects ####
options <- getOptions("GeneralizedLinearModel")
options$covariates <- c("Hours")
options$weights    <- "Turbines"
options$dependent  <- "DV"
options$modelTerms <- list(
  list(components="Hours", isNuisance=FALSE)
)
options$family     <- "binomial"
options$link       <- "logit"

options$devianceGoodnessOfFit <- TRUE
options$pearsonGoodnessOfFit  <- TRUE

options$devianceResidualVsFittedPlot  <- TRUE
options$pearsonResidualVsFittedPlot  <- TRUE
options$quantileResidualVsFittedPlot <- TRUE

options$devianceResidualVsPredictorPlot  <- TRUE
options$pearsonResidualVsPredictorPlot  <- TRUE
options$quantileResidualVsPredictorPlot <- TRUE

options$devianceResidualQqPlot   <- TRUE
options$pearsonResidualQqPlot   <- TRUE
options$quantileResidualQqPlot  <- TRUE

options$partialPlot    <- TRUE

options$workingResponseVsLinearPredictorPlot     <- TRUE

options$quantileResidualOutlierTable     <- TRUE
options$quantileResidualOutlierTableTopN <- 3

options$standardizedResidualOutlierTable      <- TRUE
options$standardizedResidualOutlierTableTopN  <- 3

options$studentizedResidualOutlierTable      <- TRUE
options$studentizedResidualOutlierTableTopN  <- 3

options$dfbetas  <- TRUE
options$dffits   <- TRUE
options$covarianceRatio <- TRUE
options$cooksDistance   <- TRUE
options$leverage <- TRUE

options$setSeed  <- TRUE
options$seed     <- 123

results <- jaspTools::runAnalysis("GeneralizedLinearModel", "turbines.csv", options)

# model summary table
test_that("Model summary table results match", {
  table <- results[["results"]][["modelSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("H\u2080", 112.67, 150.147, 150.545,  10, "", "",
                                      "H\u2081", 10.33,  49.808,  50.604,   9,  102.339, 0.000))})


# model fit table
test_that("Model fit table results match", {
  table <- results[["results"]][["modelFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Deviance", 10.331, 9, 0.3243,
                                      "Pearson",  9.251,  9, 0.4144))})

# residual plots
test_that("The deviance residual vs. fitted plot matches", {
  plotName <- results[["results"]][["diagnosticsContainer"]][["collection"]][["diagnosticsContainer_glmPlotResVsFitted"]][["collection"]][["diagnosticsContainer_glmPlotResVsFitted_devianceResidualVsFittedPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]

  jaspTools::expect_equal_plots(testPlot, "devResVsYPlot")
})

test_that("The Pearson residual vs. fitted plot matches", {
  plotName <- results[["results"]][["diagnosticsContainer"]][["collection"]][["diagnosticsContainer_glmPlotResVsFitted"]][["collection"]][["diagnosticsContainer_glmPlotResVsFitted_pearsonResidualVsFittedPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]

  jaspTools::expect_equal_plots(testPlot, "prsResVsYPlot")
})

test_that("The quantile residual vs. fitted plot matches", {
  plotName <- results[["results"]][["diagnosticsContainer"]][["collection"]][["diagnosticsContainer_glmPlotResVsFitted"]][["collection"]][["diagnosticsContainer_glmPlotResVsFitted_quantileResidualVsFittedPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]

  jaspTools::expect_equal_plots(testPlot, "quanResVsYPlot")
})

test_that("The deviance residual vs. predictor plot matches", {
  plotName <- results[["results"]][["diagnosticsContainer"]][["collection"]][["diagnosticsContainer_devianceResidualVsPredictorPlot"]][["collection"]][["diagnosticsContainer_devianceResidualVsPredictorPlot_Hours"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]

  jaspTools::expect_equal_plots(testPlot, "devResVsXPlot")
})

test_that("The Pearson residual vs. predictor plot matches", {
  plotName <- results[["results"]][["diagnosticsContainer"]][["collection"]][["diagnosticsContainer_pearsonResidualVsPredictorPlot"]][["collection"]][["diagnosticsContainer_pearsonResidualVsPredictorPlot_Hours"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]

  jaspTools::expect_equal_plots(testPlot, "prsResVsXPlot")
})

test_that("The quantile residual vs. predictor plot matches", {
  plotName <- results[["results"]][["diagnosticsContainer"]][["collection"]][["diagnosticsContainer_quantileResidualVsPredictorPlot"]][["collection"]][["diagnosticsContainer_quantileResidualVsPredictorPlot_Hours"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]

  jaspTools::expect_equal_plots(testPlot, "quanResVsXPlot")
})

test_that("The deviance residual Q-Q plot matches", {
  plotName <- results[["results"]][["diagnosticsContainer"]][["collection"]][["diagnosticsContainer_glmPlotResQQ"]][["collection"]][["diagnosticsContainer_glmPlotResQQ_devianceResidualQqPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]

  jaspTools::expect_equal_plots(testPlot, "devResQqPlot")
})

test_that("The Pearson residual Q-Q plot matches", {
  plotName <- results[["results"]][["diagnosticsContainer"]][["collection"]][["diagnosticsContainer_glmPlotResQQ"]][["collection"]][["diagnosticsContainer_glmPlotResQQ_pearsonResidualQqPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]

  jaspTools::expect_equal_plots(testPlot, "prsResQqPlot")
})

test_that("The quantile residual Q-Q plot matches", {
  plotName <- results[["results"]][["diagnosticsContainer"]][["collection"]][["diagnosticsContainer_glmPlotResQQ"]][["collection"]][["diagnosticsContainer_glmPlotResQQ_quantileResidualQqPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]

  jaspTools::expect_equal_plots(testPlot, "quanResQqPlot")
})

test_that("The partial residual plot matches", {
  plotName <- results[["results"]][["diagnosticsContainer"]][["collection"]][["diagnosticsContainer_partialResidualPlot"]][["collection"]][["diagnosticsContainer_partialResidualPlot_Hours"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]

  jaspTools::expect_equal_plots(testPlot, "partialPlot")
})

test_that("The working response vs eta plot matches", {
  plotName <- results[["results"]][["diagnosticsContainer"]][["collection"]][["diagnosticsContainer_glmPlotZVsEta"]][["collection"]][["diagnosticsContainer_glmPlotZVsEta_glmPlotZVsEta"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]

  jaspTools::expect_equal_plots(testPlot, "zVsEtaPlot")
})

# outliers table
test_that("Outlier table based on quantile residuals matches", {
  table <- results[["results"]][["diagnosticsContainer"]][["collection"]][["diagnosticsContainer_outlierTables"]][["collection"]][["diagnosticsContainer_outlierTables_quantileResidualOutlierTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9, 2.079,
                                      1, -1.325,
                                      2, 0.9768))})

test_that("Outlier table based on standardized deviance residuals matches", {
  table <- results[["results"]][["diagnosticsContainer"]][["collection"]][["diagnosticsContainer_outlierTables"]][["collection"]][["diagnosticsContainer_outlierTables_standardizedResidualOutlierTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9,  2.327,
                                      1,  -1.607,
                                      11, -1.233))})

test_that("Outlier table based on studentized deviance residuals matches", {
  table <- results[["results"]][["diagnosticsContainer"]][["collection"]][["diagnosticsContainer_outlierTables"]][["collection"]][["diagnosticsContainer_outlierTables_studentizedResidualOutlierTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9,  2.326,
                                      1,  -1.558,
                                      11, -1.240))})

# influential cases table
test_that("Influential cases table matches", {
  table <- results[["results"]][["diagnosticsContainer"]][["collection"]][["diagnosticsContainer_influenceTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(4,  -0.1795, 0.146,  -0.1918, 1.693,  0.0228, 0.2708,
                                      9,  -0.290,  0.750,  1.439,    0.3537, 0.6321, 0.1902,
                                      10, 0.1632,  -0.2761,-0.3971, 1.687, 0.0982,  0.3111))})


# multicollinearity table
test_that("Multicollinearity table matches", {
  options <- getOptions("GeneralizedLinearModel")

  options$family <- "bernoulli"
  options$link   <- "logit"
  options$covariates <- c("contNormal", "contOutlier")
  options$factors <- c("facFive")
  options$dependent <- "facGender"

  options$modelTerms <- list(
    list(components="contNormal",    isNuisance=FALSE),
    list(components="contOutlier",   isNuisance=FALSE),
    list(components="facFive", isNuisance=FALSE)
  )

  options$tolerance <- TRUE
  options$vif       <- TRUE
  results <- jaspTools::runAnalysis("GeneralizedLinearModel", "debug.csv", options)

  table <- results[["results"]][["diagnosticsContainer"]][["collection"]][["diagnosticsContainer_multicolliTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("contNormal",  0.9537, 1.049,
                                      "contOutlier", 0.9743, 1.026,
                                      "facFive",     0.9377, 1.066))})

# estimated marginal means table and contrast table
test_that("Estimated marginal means table matches", {
  options <- getOptions("GeneralizedLinearModel")

  options$family <- "bernoulli"
  options$link   <- "logit"
  options$covariates <- c("contNormal", "contOutlier")
  options$factors <- c("facFive")
  options$dependent <- "facGender"

  options$modelTerms <- list(
    list(components="contNormal",    isNuisance=FALSE),
    list(components="contOutlier",   isNuisance=FALSE),
    list(components="facFive", isNuisance=FALSE)
  )

  options$marginalMeansVars <- c("contNormal")
  options$marginalMeansComparison <- TRUE
  options$marginalMeansComparisonWith <- 0
  options$marginalMeansCi <- TRUE

  options$marginalMeansContrast <- TRUE
  options$contrasts <- list(list(isContrast = FALSE, levels = c("1", "2", "3"), name = "contNormal", values = c("-1", "0", "1")),
                            list(isContrast = TRUE,  levels = c("1", "2", "3"), name = "Contrast 1", values = c("1", "0", "-1")))

  results <- jaspTools::runAnalysis("GeneralizedLinearModel", "debug.csv", options)

  emmTable <- results[["results"]][["emmContainer"]][["collection"]][["emmContainer_emmSummary"]][["data"]]
  jaspTools::expect_equal_tables(emmTable,
                                 list(-1.247,  0.3772, 0.0723, 0.2489, 0.5254, -1.630, 0.1031, 1,
                                      -0.1887, 0.5023, 0.052,  0.4017, 0.6026, 0.0436, 0.9652, 2,
                                      0.8697,  0.6271, 0.0752, 0.4723, 0.7596, 1.615,  0.1063, 3))

  contrastsTable <- results[["results"]][["emmContainer"]][["collection"]][["emmContainer_contrastsTable"]][["data"]]
  jaspTools::expect_equal_tables(contrastsTable,
                                 list("Contrast 1",  -0.2499, 0.1108, Inf, -2.255, 0.0242))

})

# test error handling
test_that("Input error handling", {

  # check for bernoulli if dv is factor with two levels
  options <- getOptions("GeneralizedLinearModel")
  options$family <- "bernoulli"
  options$link   <- "logit"
  options$covariates <- c("contNormal", "contOutlier")
  options$dependent <- "facFive"
  options$modelTerms <- list(
    list(components="contNormal",    isNuisance=FALSE),
    list(components="contOutlier",   isNuisance=FALSE)
  )
  results <- jaspTools::runAnalysis("GeneralizedLinearModel", "debug.csv", options)
  expect_identical(results$status, "validationError", "Bernoulli dv factor check")

  # check if weights are non-negative
  options <- getOptions("GeneralizedLinearModel")
  options$family  <- "bernoulli"
  options$link    <- "logit"
  options$weights <- "contcor1"
  options$covariates <- c("contNormal", "contOutlier")
  options$dependent <- "facGender"
  options$modelTerms <- list(
    list(components="contNormal",    isNuisance=FALSE),
    list(components="contOutlier",   isNuisance=FALSE)
  )
  results <- jaspTools::runAnalysis("GeneralizedLinearModel", "debug.csv", options)
  expect_identical(results$status, "validationError", "Weights non-negative check")
})


# test offset (in gamma regression)
test_that("Gamma regression (with an offset term) results match", {
  options <- getOptions("GeneralizedLinearModel")
  options$covariates <- c("logOfHeight")
  options$dependent  <- "Volume"
  options$modelTerms <- list(
    list(components="logOfHeight", isNuisance=FALSE)
  )
  options$family     <- "gamma"
  options$link       <- "log"
  options$offset     <- "twoTimesLogOfGirth"

  results <- jaspTools::runAnalysis("GeneralizedLinearModel", "trees.csv", options)
  table <- results[["results"]][["estimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table, list("(Intercept)", -6.617, 0.7275, -9.096, .000,
                                             "logOfHeight",       1.104,  0.1681, 6.570, .000))

})

# test intercept-only model for confidence intervals
test_that("Intercept-only binomial regression results match", {
  options <- getOptions("GeneralizedLinearModel")
  options$weights    <- "Turbines"
  options$dependent  <- "DV"
  options$family     <- "binomial"
  options$coefficientCi <- TRUE
  options$coefficientCiLevel <- 0.95

  options$link       <- "logit"
  results <- jaspTools::runAnalysis("GeneralizedLinearModel", "turbines.csv", options)
  table <- results[["results"]][["estimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table, list("(Intercept)", -1.1235, 0.1118, -10.05, .000, -1.347, -0.9082))

})
