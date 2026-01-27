context("GeneralizedLinearModel")

## Load some typed test data:
testData <- readRDS("testData.rds")

getOptions <- function(analysisName) {
  options <- jaspTools::analysisOptions(analysisName)
  options <- addCommonQmlOptions(options)
  return(options)
}

addCommonQmlOptions <- function(options) {
  # jaspTools doesn't recognize common QML elements so this function adds the defaults manually
  #root <- testthat::test_path(file.path("..", "..", "inst", "qml", "common"))
  root <- testthat::test_path("..", "..", "inst", "qml", "common")
  c(
    options,
    jaspTools:::readQML(testthat::test_path(root, "GlmInputComponent.qml")),
    jaspTools:::readQML(testthat::test_path(root, "GlmResidualAnalysisPlotsComponent.qml")),
    jaspTools:::readQML(testthat::test_path(root, "EmmComponent.qml")),
    jaspTools:::readQML(testthat::test_path(root, "OutlierComponent.qml"))
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

options$partialResidualPlot    <- TRUE

options$workingResponseVsLinearPredictorPlot     <- TRUE

options$quantileResidualOutlierTable     <- TRUE
options$quantileResidualOutlierTableTopN <- 3

options$standardizedResidualOutlierTable      <- TRUE
options$standardizedResidualOutlierTableTopN  <- 3

options$studentizedResidualOutlierTable      <- TRUE
options$studentizedResidualOutlierTableTopN  <- 3

options$residualCasewiseDiagnostic <- TRUE
options$residualCasewiseDiagnosticType <- "outliersOutside"
options$residualCasewiseDiagnosticZThreshold <- 1


options$dfbetas  <- TRUE
options$dffits   <- TRUE
options$covarianceRatio <- TRUE
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
                                 list(-0.607898874134593, 0.562520030505165, 1, 0.0910041875476176,
                                      0.811136811632276, 0, -0.608545530898244, 0.122014731899716,
                                      0.0286397537025298, -1.02948417315995, -1.60670300282295, -0.182578461118574,
                                      0.0432371780205531, 7, 0.0954834805489629, 1.14396823117325,
                                      0.214285714285714, -0.421414814442793, 0.141203925314399, 0.283760332058426,
                                      -0.341834673218477, -1.10858029927343, 0, -0.290005764938072,
                                      0.750048649952999, 9, 0.632130380240399, 0.353736621049185,
                                      0.647058823529412, 1.43918415935164, 0.190243757445277, 0.468418331001346,
                                      0.717424210957795, 2.32732272718569, 0.484997819985084, -0.707619585051628,
                                      11, 0.447283433716663, 1.44554646273235, 0.583333333333333,
                                      -0.886667016095175, 0.36304481969478, 0.662151029321139, -0.352325499848782,
                                      -1.2326217135089))})


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
  results <- jaspTools::runAnalysis("GeneralizedLinearModel", testData, options)

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

  results <- jaspTools::runAnalysis("GeneralizedLinearModel", testData, options)

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


# test multinomial logistic regression
test_that("Multinomial logistic regression results match", {
  options <- getOptions("GeneralizedLinearModel")
  options$covariates <- c("contNormal")
  options$dependent  <- "facFive"
  options$modelTerms <- list(
    list(components="contNormal", isNuisance=FALSE)
  )
  options$devianceGoodnessOfFit <- TRUE
  options$pearsonGoodnessOfFit  <- TRUE
  options$coefficientCi <- TRUE
  options$coefficientCiLevel <- 0.95
  options$family     <- "other"
  options$otherGlmModel <- "multinomialLogistic"

  results <- jaspTools::runAnalysis("GeneralizedLinearModel", testData, options)
  table <- results[["results"]][["estimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table, list(-0.694719007279079, 0.582071603027983, -0.0563237021255478, "(Intercept)<unicode><unicode><unicode>1",
                                             0.862712942200181, 0.325717875526852, -0.172921741044895, -0.717808103822768,
                                             0.570581627970062, -0.0736132379263526, "(Intercept)<unicode><unicode><unicode>2",
                                             0.822781889719081, 0.328676889462124, -0.223968402666886, -0.630295504485506,
                                             0.614094914233988, -0.00810029512575909, "(Intercept)<unicode><unicode><unicode>3",
                                             0.979642932998874, 0.317452368649396, -0.0255165685492343, -0.704151601955603,
                                             0.577382116241445, -0.0633847428570791, "(Intercept)<unicode><unicode><unicode>4",
                                             0.84626992424874, 0.326927874263411, -0.19387989782116, -0.930384360212812,
                                             0.29749919990225, -0.316442580155281, "contNormal<unicode><unicode><unicode>1",
                                             0.312390059031085, 0.313241357953628, -1.01021966646603, -0.98463420118505,
                                             0.256038172468671, -0.364298014358189, "contNormal<unicode><unicode><unicode>2",
                                             0.249729558435057, 0.31650387033639, -1.15100650734856, -0.439350709251133,
                                             0.681154160098849, 0.120901725423858, "contNormal<unicode><unicode><unicode>3",
                                             0.67232619342689, 0.285848331445981, 0.422957604168159, -0.953382729130251,
                                             0.279941825642027, -0.336720451744112, "contNormal<unicode><unicode><unicode>4",
                                             0.284523464240265, 0.314629392300212, -1.07021295525633))

})


# test ordinal logistic regression
test_that("Ordinal logistic regression results match", {
  options <- getOptions("GeneralizedLinearModel")
  options$covariates <- c("contNormal")
  options$dependent  <- "facFive"
  options$modelTerms <- list(
    list(components="contNormal", isNuisance=FALSE)
  )
  options$devianceGoodnessOfFit <- TRUE
  options$pearsonGoodnessOfFit  <- TRUE
  options$coefficientCi <- TRUE
  options$coefficientCiLevel <- 0.95
  options$family     <- "other"
  options$otherGlmModel <- "ordinalLogistic"

  results <- jaspTools::runAnalysis("GeneralizedLinearModel", testData, options)
  table <- results[["results"]][["estimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table, list(-1.92119305349926, -0.925351795497781, -1.42327242449852, "(Intercept)<unicode><unicode><unicode>1",
                                             2.11374210463909e-08, 0.254045805396564, -5.60242442215016,
                                             -0.840434215981626, -0.0264133113280063, -0.433423763654816,
                                             "(Intercept)<unicode><unicode><unicode>2", 0.036873889007028,
                                             0.207662209886129, -2.08715761954225, -0.0234567208898863, 0.78657180442429,
                                             0.381557541767202, "(Intercept)<unicode><unicode><unicode>3",
                                             0.0648267301248355, 0.206643727054063, 1.84645112245472, 0.871446124139646,
                                             1.8561073410799, 1.36377673260977, "(Intercept)<unicode><unicode><unicode>4",
                                             5.66124127361787e-08, 0.251193701697361, 5.42918362759291, -0.491644444603388,
                                             0.170180146671655, -0.160732148965867, "contNormal", 0.341095881967457,
                                             0.168835906296093, -0.952002168803985))

})


# test firth logistic regression
test_that("Firth logistic regression results match", {
  options <- getOptions("GeneralizedLinearModel")
  options$covariates <- c("contNormal")
  options$dependent  <- "facGender"
  options$modelTerms <- list(
    list(components="contNormal", isNuisance=FALSE)
  )
  options$devianceGoodnessOfFit <- TRUE
  options$pearsonGoodnessOfFit  <- TRUE
  options$coefficientCi <- TRUE
  options$coefficientCiLevel <- 0.95
  options$family     <- "other"
  options$otherGlmModel <- "firthLogistic"

  results <- jaspTools::runAnalysis("GeneralizedLinearModel", "debug.csv", options)
  table <- results[["results"]][["estimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table, list(-0.321648430817825, 0.49637552658174, 0.0833656241411372, "(Intercept)",
                                             0.687684018886119, 0.207349730577828, 0.161604008071805, 0.0438031690299891,
                                             0.864979794866346, 0.429594461170132, "contNormal", 0.0283926079313983,
                                             0.205791561077245, 4.80406875037797))

})
