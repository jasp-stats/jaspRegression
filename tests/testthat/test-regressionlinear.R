context("Linear Regression")

# Some of the sample files used in this file below are distributed under the following license restrictions:
# This data set comes from Field, A. P. (2017). {Discovering Statistics Using IBM SPSS Statistics} (5th ed.). London: Sage. The data set was constructed by Andy Field who therefore owns the copyright. Andy Field generously agreed that we can include the data set in the JASP data library. This data set is also publicly available on the website that accompanies Andy Field`s book, {https://edge.sagepub.com/field5e}. Without Andy Field`s explicit consent, this data set may not be distributed for commercial purposes, this data set may not be edited, and this data set may not be presented without acknowledging its source (i.e., the terms of a CC BY-NC-ND license see https://creativecommons.org/licenses/by-nc-nd/3.0/).

# does not test
# - stepwise methods (currently gives an error if I set p entry too high)
# - plots handle errors

## Load some typed test data:
testData <- readRDS("testData.rds")

initOptsLinReg <- function() {
  options <-  jaspTools::analysisOptions("RegressionLinear")
  
  options$dependent <- "contNormal"
  options$covariates <- "contGamma"
  options$modelTerms <- list(
    list(components= NULL, name = "model0", title = "Model 0"),
    list(components= "contGamma", name = "model1", title = "Model 1")
  )
  
  options$rSquaredChange <- TRUE
  options$fChange <- TRUE
  options$residualDurbinWatson <- FALSE
  options$residualCasewiseDiagnostic <- FALSE
  options$residualsSavedToData <- FALSE
  options$residualsSavedToDataColumn <- FALSE
  options$residualStatistic <- FALSE
  
  return(options)
}

test_that("Main table results match", {
  options <- initOptsLinReg()
  
  options$weights <- "facFifty"
  options$residualDurbinWatson <- TRUE
  
  results <- jaspTools::runAnalysis("RegressionLinear", "test.csv", options)
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(2.22949319138764, -0.124606080290573, "", 0, 0, 0, 4.99355368556631,
                           0, 0, 99, "H<unicode>", "", 2.22918408630401, -0.124437281020818,
                           0.000152821712396024, 0.00124876050417603, 1.55940279678998e-06,
                           1.55940279678998e-06, 5.01896242334011, -0.0102025063175828,
                           1, 98, "H<unicode>", 0.990161847660694)
  )
})

test_that("Coefficients table results match", {
  options <- initOptsLinReg()

  options$coefficientEstimate <- TRUE
  options$coefficientCi <- TRUE
  options$coefficientCiLevel <- 0.9
  options$collinearityDiagnostic <- TRUE
  options$collinearityStatistic <- TRUE
  options$vovkSellke <- TRUE
 
  results <- jaspTools::runAnalysis("RegressionLinear", "test.csv", options)
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_coeffTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list("FALSE", 0.105841360919316, -0.364486647151235, "H<unicode>",
                           "(Intercept)", 0.0775993871437191, -1.78331595418435, -0.18874858754,
                           -0.013010527928765, 1.85461222592575, "TRUE", 0.176988347288719,
                           -0.399521419778159, "H<unicode>", "(Intercept)", 0.552030096201664,
                           -0.596780555892316, -0.105623204281424, 0.188275011215311, 1,
                           "FALSE", 0.0696473684093105, 1, -0.156541849851968, "H<unicode>",
                           "contGamma", 0.558497687623533, -0.0592003859505643, -0.587083595666713,
                           1, -0.0408888274744623, 0.0747641949030438, 1)
  )
})

test_that("ANOVA table results match", {
  options <- initOptsLinReg()
  
  options$dependent <- "debCollin1"
  options$modelFit <- TRUE
  options$vovkSellke <- TRUE
  
  results <- jaspTools::runAnalysis("RegressionLinear", "test.csv", options)
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_anovaTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list("TRUE", 2.93169085062604, 0.0193710317063457, 0.0193710317063457,
                           "Regression", 1, "H<unicode>", 0.0900191505810211, 1.69731445510183,
                           "FALSE", 0.00660746057252562, 0.647531136107511, "Residual",
                           98, "H<unicode>", "FALSE", 0.666902167813857, "Total", 99, "H<unicode>"))
})

test_that("Coefficients Covariance table results match", {
  options <- initOptsLinReg()
  options$covariates <- c("contGamma", "contcor1")
  options$modelTerms <- list(
    list(components=list("contGamma"), name="model0", title = "Model 0"),
    list(components=list("contGamma", "contcor1"), name="model1", title = "Model 1")
  )
  options$covarianceMatrix <- TRUE
  results <- jaspTools::runAnalysis("RegressionLinear", "test.csv", options)
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_coeffCovMatrixTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("FALSE", 0.00485075592634222, "", "M<unicode>", "contGamma", "TRUE",
         0.00490486111017858, 0.00116294327838645, "M<unicode>", "contGamma",
         "FALSE", "", 0.0112500585702943, "M<unicode>", "contcor1")

  )
})

test_that("Descriptive table results match", {
  options <- initOptsLinReg()
  
  options$descriptives <- TRUE
  
  results <- jaspTools::runAnalysis("RegressionLinear", "test.csv", options)
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_descriptivesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("contNormal", 100, -0.18874858754, 1.05841360919316, 0.105841360919316,
         "contGamma", 100, 2.03296079621, 1.53241112621044, 0.153241112621044)
  )
})

test_that("Part and Partial Correlations table results match", {
  options <- initOptsLinReg()
  
  options$covariates <- c("debCollin2", "contGamma")
  options$modelTerms <- list(
    list(components=list("debCollin2"), name="model0", title = "Model 0"),
    list(components=list("contGamma", "debCollin2"), name="model1", title = "Model 1")
  )
  options$partAndPartialCorrelation <- TRUE
  
  results <- jaspTools::runAnalysis("RegressionLinear", "test.csv", options)
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_partialCorTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("FALSE", "M<unicode>", "debCollin2", -0.0094541786161782, -0.0094541786161782,
                                      "TRUE", "M<unicode>", "contGamma", -0.0617145529439822, -0.0617173111983367,
                                      "FALSE", "M<unicode>", "debCollin2", -0.0198338559459935, -0.0198687032851424
                                 )
  )
})

test_that("Collinearity Diagonistic table results match", {
  options <- initOptsLinReg()

  options$covariates <- "contcor1"
  options$modelTerms <- list(
    list(components=NULL, name="model0", title = "Model 0"),
    list(components="contcor1", name="model1", title = "Model 1")
  )
  options$collinearityDiagnostic <- TRUE
  results <- jaspTools::runAnalysis("RegressionLinear", "test.csv", options)
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_collinearityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.473937737611082, "TRUE", 0.473937737611089, 1, 1, 1.05212452477783,
         "H<unicode>", 0.526062262388918, "FALSE", 0.526062262388911,
         1.0535567372186, 2, 0.947875475222171, "H<unicode>")
  )
})

test_that("Residuals Statistics table results match", {
  options <- initOptsLinReg()
  options$dependent <- "contNormal"
  options$covariates <- "contcor1"
  options$modelTerms <- list(
    list(components=NULL, name="model0", title = "Model 0"),
    list(components="contcor1", name="model1", title = "Model 1")
  )
  options$residualStatistic <- TRUE
  results <- jaspTools::runAnalysis("RegressionLinear", "test.csv", options)
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_residualsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Predicted Value", -0.559288923489434, 0.200246244240391, -0.18874858754,
         0.170438384014894, 100, "Residual", -2.87689451816188, 3.15584820375961,
         9.84238732182341e-18, 1.04460046208093, 100, "Std. Predicted Value",
         -2.17404276678107, 2.28231940844029, 6.76021738588162e-17, 1,
         100, "Std. Residual", -2.75476210882495, 3.10457788320998, 0.000645077391284091,
         1.00643569133304, 100)
  )
})

test_that("Casewise Diagnostics table results match", {
  options <- initOptsLinReg()
  options$dependent <- "contNormal"
  options$covariates <- "contOutlier"
  options$modelTerms <- list(
    list(components=NULL, name="model0", title = "Model 0"),
    list(components="contOutlier", name="model1", title = "Model 1")
  )
  options$residualCasewiseDiagnostic <- TRUE
  options$residualCasewiseDiagnosticType <- "outliersOutside"
  options$residualCasewiseDiagnosticZThreshold <- 3
  results <- jaspTools::runAnalysis("RegressionLinear", "test.csv", options)
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_influenceTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(55, 0.0577123260439598, 3.356094448, -0.187237305306683, 3.54333175330668,
                           3.34810934796609, 0, 83, 1.15289593050314, 2.958797116, -0.143545494526367,
                           3.10234261052637, 3.22377600371253)
  )
})

test_that("Residuals vs. Dependent plot matches", {
  options <- initOptsLinReg()

  options$residualVsDependentPlot <- TRUE
  
  results <- jaspTools::runAnalysis("RegressionLinear", "test.csv", options)

  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "residuals-dependent")
})

test_that("Residuals vs. Covariates plot matches", {
  options <- initOptsLinReg()
  
  options$residualVsCovariatePlot <- TRUE
  
  results <- jaspTools::runAnalysis("RegressionLinear", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "residuals-covariates")
})

test_that("Residuals vs. Predicted plot matches", {
  options <- initOptsLinReg()
  
  options$residualVsFittedPlot <- TRUE
  
  results <- jaspTools::runAnalysis("RegressionLinear", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "residuals-predicted")
})

test_that("Standardized Residuals Histogram matches", {
  options <- initOptsLinReg()
  
  options$residualHistogramPlot <- TRUE
  options$residualHistogramStandardizedPlot <- TRUE
  
  results <- jaspTools::runAnalysis("RegressionLinear", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "residuals-histogram")
})

test_that("Q-Q Plot Standardized Residuals matches", {
  options <- initOptsLinReg()
  
  options$residualQqPlot <- TRUE
  results <- jaspTools::runAnalysis("RegressionLinear", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "residuals-q-q")
})

test_that("Marginal effects plot matches", {
  options <- initOptsLinReg()

  options$covariates <- "contcor1"
  options$modelTerms <- list(
    list(components=NULL, name="model0", title = "Model 0"),
    list(components="contcor1", name="model1", title = "Model 1")
  )
  options$marginalPlot <- TRUE
  options$marginalPlotCi <- TRUE
  options$marginalPlotCiLevel <- 0.95
  options$marginalPlotPredictionInterval <- TRUE
  options$marginalPlotPredictionIntervalLevel <- 0.95

  results <- jaspTools::runAnalysis("RegressionLinear", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "marginal-plot")
})

test_that("Analysis handles errors", {
  options <- initOptsLinReg()
  
  options$dependent <- "debInf"
  options$covariates <- "contGamma"
  options$modelTerms <- list(
    list(components=NULL, name="model0", title = "Model 0"),
    list(components="contGamma", name="model1", title = "Model 1")
  )  
  results <- jaspTools::runAnalysis("RegressionLinear", "test.csv", options)
  expect_identical(results[["status"]], "validationError", label="Inf dependent check")

  options$dependent <- "contNormal"
  options$covariates <- "debInf"
  options$modelTerms <- list(
    list(components=NULL, name="model0", title = "Model 0"),
    list(components=list(options$covariates), name="model1", title = "Model 1")
  ) 
  options$modelTerms <- list(list(components="debInf", name="model0", title = "Model 0"))
  results <- jaspTools::runAnalysis("RegressionLinear", "test.csv", options)
  expect_identical(results[["status"]], "validationError", label="Inf covariate check")

  options$covariates <- "contGamma"
  options$weights <- "debInf"
  options$modelTerms <- list(
    list(components=NULL, name="model0", title = "Model 0"),
    list(components=list(options$covariates), name="model1", title = "Model 1")
  ) 
  results <- jaspTools::runAnalysis("RegressionLinear", "test.csv", options)
  expect_identical(results[["status"]], "validationError", label="Inf weights check")

  options$dependent <- "debSame"
  options$covariates <- "contGamma"
  options$weights <- ""
  options$modelTerms <- list(
    list(components=NULL, name="model0", title = "Model 0"),
    list(components=list(options$covariates), name="model1", title = "Model 1")
  ) 
  results <- jaspTools::runAnalysis("RegressionLinear", "test.csv", options)
  expect_identical(results[["status"]], "validationError", label="No variance dependent check")

  options$dependent <- "contNormal"
  options$covariates <- "debSame"
  options$modelTerms <- list(
    list(components=NULL, name="model0", title = "Model 0"),
    list(components=list(options$covariates), name="model1", title = "Model 1")
  ) 
  results <- jaspTools::runAnalysis("RegressionLinear", "test.csv", options)
  expect_identical(results[["status"]], "validationError", label="No variance covariate check")

  options$dependent <- "contGamma"
  options$covariates <- "contcor1"
  options$weights <- "contNormal"
  options$modelTerms <- list(
    list(components=NULL, name="model0", title = "Model 0"),
    list(components=list(options$covariates), name="model1", title = "Model 1")
  ) 
  results <- jaspTools::runAnalysis("RegressionLinear", "test.csv", options)
  expect_identical(results[["status"]], "validationError", label="Negative weights check")

  options$dependent <- "debNaN"
  options$covariates <- "contcor1"
  options$weights <- ""
  options$modelTerms <- list(
    list(components=NULL, name="model0", title = "Model 0"),
    list(components=list(options$covariates), name="model1", title = "Model 1")
  ) 
  results <- jaspTools::runAnalysis("RegressionLinear", "test.csv", options)
  expect_identical(results[["status"]], "validationError", label="Too few obs dependent check")

  options$dependent <- "contGamma"
  options$covariates <- "debNaN"
  options$modelTerms <- list(
    list(components=NULL, name="model0", title = "Model 0"),
    list(components=list(options$covariates), name="model1", title = "Model 1")
  ) 
  results <- jaspTools::runAnalysis("RegressionLinear", "test.csv", options)
  expect_identical(results[["status"]], "validationError", label="Too few obs covariate check")

  options$dependent <- "contGamma"
  options$covariates <- "contNormal"
  options$weights <- "debNaN"
  options$modelTerms <- list(
    list(components=NULL, name="model0", title = "Model 0"),
    list(components=list(options$covariates), name="model1", title = "Model 1")
  ) 
  results <- jaspTools::runAnalysis("RegressionLinear", "test.csv", options)
  expect_identical(results[["status"]], "validationError", label="Too few obs weights check")

  options <- jaspTools::analysisOptions("RegressionLinear")
  options$dependent <- "contNormal"
  options$covariates <- c("debCollin2", "debCollin3")
  options$modelTerms <- list(
    list(components=list("debCollin2"), name="model0", title = "Model 0"),
    list(components=list("debCollin2", "debCollin3"), name="model1", title = "Model 1")
  ) 
  results <- jaspTools::runAnalysis("RegressionLinear", "test.csv", options)
  results$results$errorMessage
  expect_identical(results[["status"]], "validationError", label="Perfect correlation check")
})

test_that("Analysis handles categorical predictors in model summary table", {
  options <- initOptsLinReg()
  options$dependent <- "contNormal"
  options$covariates <- "contcor1"
  options$factors <- "facFive"
  options$modelTerms <- list(
    list(components = list("contcor1"), name ="model0", title = "Model 0"),
    list(components = list("contcor1", "facFive"), name="model1", title = "Model 1")
  ) 
  options$rSquaredChange <- TRUE
  options$fChange <- TRUE

  results <- jaspTools::runAnalysis("RegressionLinear", testData, options)

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(2.60891821036142, 0.161031927910319, 0.0259312818065142, 0.0259312818065142,
                                      1.04991652929926, 0.0159918050902543, 1, 98, "H<unicode>", 0.109479317429059,
                                      1.1726522384496, 0.26875124434343, 0.0722272313361422, 0.046295949529628,
                                      1.04623657078777, 0.0228776159816817, 4, 94, "H<unicode>", 0.327916402792662
                                 )
                                 )
})

test_that("Part And Partial Correlations table results match", {
  # Part and partial correlations, including categorical predictors, verified with SPSS,
  # see pdf doc in https://github.com/jasp-stats/jasp-issues/issues/1638
  options <- initOptsLinReg()
  options$covariates <- c("education", "prestige")
  options$dependent <- "income"
  options$factors <- "occup_type"
  options$modelTerms <- list(
    list(components=list("education", "prestige", "occup_type"), name="model0", title = "Model 0")
  ) 
  options$partAndPartialCorrelation <- TRUE
  set.seed(1)
  results <- jaspTools::runAnalysis("RegressionLinear", "Duncan.csv", options)

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_partialCorTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("FALSE", "M<unicode>", "education", -0.107546637580271, -0.218295377900751,
                                      "FALSE", "M<unicode>", "prestige", 0.363479641784282, 0.603066145085563,
                                      "FALSE", "M<unicode>", "occup_type", 0.236073658532064, 0.44075211401084
                                 ))
})

test_that("Bootstrapping runs", {
  options <- initOptsLinReg()
  options$dependent <- "contNormal"
  options$covariates <- "contcor1"
  options$factors <- "facFive"
  options$modelTerms <- list(
    list(components=list("contcor1"), name="model0", title = "Model 0"),
    list(components=list("contcor1", "facFive"), name="model1", title = "Model 1")
  ) 
  options$coefficientBootstrap <- TRUE
  options$coefficientBootstrapSamples <- 100
  options$coefficientCi <- TRUE

  set.seed(1)
  results <- jaspTools::runAnalysis("RegressionLinear", testData, options)

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_bootstrapCoeffTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("FALSE", 0.0998957223362395, -0.000889223735744071, -0.400661842088638,
                                      "H<unicode>", "(Intercept)", 0.1200000001, -0.198940836903414,
                                      0.0235981427404004, "FALSE", 0.112187827694195, -0.00253188343531965,
                                      -0.0441455978813581, "H<unicode>", "contcor1", 0.1800000001,
                                      0.176524255654758, 0.394786085624383, "TRUE", 0.175150315273731,
                                      -0.00289810869532725, -0.654200114073205, "H<unicode>", "(Intercept)",
                                      0.0700000001, -0.361576874223558, 0.0143680426079194, "FALSE",
                                      0.116836035315656, 0.0113358413902619, -0.0430592781366748,
                                      "H<unicode>", "contcor1", 0.1200000001, 0.191798786560478, 0.443064395802951,
                                      "FALSE", 0.274053233142539, -0.0338647998948993, -0.663094065407741,
                                      "H<unicode>", "facFive (2)", 0.9900000001, -0.0182010680002372,
                                      0.520022559578832, "FALSE", 0.321752383164526, -0.0110564722644724,
                                      -0.0861099530443645, "H<unicode>", "facFive (3)", 0.1200000001,
                                      0.529159014118249, 1.11379895505474, "FALSE", 0.326698225179954,
                                      -0.0131956271183243, -0.714263511271674, "H<unicode>", "facFive (4)",
                                      0.9200000001, -0.0271369694556396, 0.636248056838376, "FALSE",
                                      0.336066363446404, 0.0515283211744709, -0.404736238389874, "H<unicode>",
                                      "facFive (5)", 0.6200000001, 0.385401773180763, 0.837810349834156
                                      )
                                 )
})

test_that("Marginal effects plots works with interactions", {
  options <- initOptsLinReg()
  options$.meta <- list(covariates = list(shouldEncode = TRUE), dependent = list(
    shouldEncode = TRUE), factors = list(shouldEncode = TRUE),
    modelTerms = list(shouldEncode = TRUE), weights = list(shouldEncode = TRUE))
  options$covariates <- c("contGamma", "contcor1")
  options$dependent <- "contNormal"
  options$marginalPlot <- TRUE
  options$modelTerms <- list(
    list(components=NULL, name="model0", title = "Model 0"),
    list(components=list("contGamma", "contcor1", list("contGamma", "contcor1")), name="model1", title = "Model 1")
  ) 
  set.seed(1)
  results <- runAnalysis("RegressionLinear", "test.csv", options)

  testthat::expect_equal(results[["status"]], "complete")

  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_marginalPlotsContainer"]][["collection"]][["modelContainer_marginalPlotsContainer_contGamma"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "marginal-effect-of-contgamma-on-contnormal")

  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_marginalPlotsContainer"]][["collection"]][["modelContainer_marginalPlotsContainer_contcor1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "marginal-effect-of-contcor1-on-contnormal")
})

# Below are the unit tests for Andy Field's book ----

# Chapter 1
test_that("Fields Book - Chapter 1 results match", {
  options <- initOptsLinReg()
  options$dependent <- "sales"
  options$covariates <- "adverts"
  options$modelTerms <- list(
    list(components=NULL, name="model0", title = "Model 0"),
    list(components=list("adverts"), name="model1", title = "Model 1")
  )
  options$fChange <- FALSE
  options$rSquaredChange <- FALSE
  
  results <- jaspTools::runAnalysis("RegressionLinear", dataset = "Album Sales.csv", options)
  output1 <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(output1,
                      list(0, 0, 80.698956672563, 0, "H<unicode>", 0.578487741981689, 0.334648067623073,
                           65.9914352978124, 0.331287704328241, "H<unicode>")
  )
  output2 <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_anovaTable"]][["data"]]
  jaspTools::expect_equal_tables(output2,
                      list("TRUE", 99.5868714961988, 433687.832532257, 433687.832532257,
                           "Regression", 1, "H<unicode>", 2.94197985216586e-19, "FALSE",
                           4354.86953266536, 862264.167467742, "Residual", 198, "H<unicode>",
                           "FALSE", 1295952, "Total", 199, "H<unicode>")

  )
  output3 <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_coeffTable"]][["data"]]
  jaspTools::expect_equal_tables(output3,
                      list("FALSE", 5.70627794978487, "H<unicode>", "(Intercept)", 1.59908972195556e-84,
                           33.8574464300821, 193.2, "TRUE", 7.53657467947199, "H<unicode>",
                           "(Intercept)", 5.9678171979745e-43, 17.7985283125294, 134.139937812074,
                           "FALSE", 0.00963236621523019, "H<unicode>", "adverts", 2.94197985216575e-19,
                           0.578487741981689, 9.97932219623151, 0.0961244859738772)
  )

  options$covariates <- c("adverts", "airplay", "attract")
  options$modelTerms <- list(
    list(components=list("adverts"), name="model0", title = "Model 0"),
    list(components=list("adverts", "airplay", "attract"), name="model1", title = "Model 1")
  ) 
  results <- jaspTools::runAnalysis("RegressionLinear", dataset = "Album Sales.csv", options)
  output4 <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(output4,
                      list("H<unicode>", 0.5784877, 0.3346481, 0.3312877, 65.99144,
                           "H<unicode>", 0.8152715, 0.6646677, 0.659535, 47.08734)
  )
  output5 <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_anovaTable"]][["data"]]
  jaspTools::expect_equal_tables(output5,
                      list("FALSE", 99.5868714961988, 433687.832532257, 433687.832532257,
                           "Regression", 1, "H<unicode>", 2.94197985216586e-19, "FALSE",
                           4354.86953266536, 862264.167467742, "Residual", 198, "H<unicode>",
                           "FALSE", 1295952, "Total", 199, "H<unicode>", "TRUE", 129.498273390974,
                           287125.806089999, 861377.418269998, "Regression", 3, "H<unicode>",
                           2.87553505648734e-46, "FALSE", 2217.2172537245, 434574.581730001,
                           "Residual", 196, "H<unicode>", "FALSE", 1295952, "Total", 199,
                           "H<unicode>")
  )
  output6 <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_coeffTable"]][["data"]]
  jaspTools::expect_equal_tables(output6,
                      list("FALSE", 7.53657467947199, "H<unicode>", "(Intercept)", 5.9678171979745e-43,
                           17.7985283125294, 134.139937812074, "FALSE", 0.00963236621523019,
                           "H<unicode>", "adverts", 2.94197985216575e-19, 0.578487741981689,
                           9.97932219623151, 0.0961244859738772, "TRUE", 17.3500005649351,
                           "H<unicode>", "(Intercept)", 0.126669773576493, -1.53388804006521,
                           -26.6129583616786, "FALSE", 0.00692301687199933, "H<unicode>",
                           "adverts", 5.05493680123644e-26, 0.510846225434074, 12.2612477656671,
                           0.0848848251534775, "FALSE", 0.277770831682515, "H<unicode>",
                           "airplay", 1.32630717054857e-25, 0.511988143597586, 12.1230337617277,
                           3.36742517051031, "FALSE", 2.43784926487601, "H<unicode>", "attract",
                           9.49212095293172e-06, 0.191683427305029, 4.54758846836075, 11.0863352045519)
  )
})

# Chapter 2
test_that("Fields Book - Chapter 2 results match", {
  options <- initOptsLinReg()
  options$dependent <- "sales"
  options$covariates <- c("adverts", "airplay", "attract")
  options$modelTerms <- list(
    list(components=list("adverts"), name="model0", title = "Model 0"),
    list(components=list("adverts", "airplay", "attract"), name="model1", title = "Model 1")
  ) 
  options$rSquaredChange <- TRUE
  options$fChange <- TRUE
  
  options$coefficientCi <- TRUE
  results <- jaspTools::runAnalysis("RegressionLinear", dataset = "Album Sales.csv", options)
  output4 <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(output4,
                      list("H<unicode>", 0.5784877, 0.3346481, 0.3312877, 65.99144, 0.3346481, 99.58687, 1, 198, 2.94198e-19,
                           "H<unicode>", 0.8152715, 0.6646677, 0.659535, 47.08734, 0.3300196, 96.44738, 2, 196, 6.879395e-30)
  )
  # needs investigating
  # output6 <- results[["results"]][["regression"]][["data"]]
  # expect_equal_tables(output6,
  #                     list(0, "(Intercept)", 134.1399, 7.536575, "", 17.79853, 5.967817e-43, 119.2777, 149.0022, "TRUE",
  #                          "", "adverts", 0.09612449, 0.009632366, 0.5784877, 9.979322, 2.94198e-19, 0.07712929, 0.1151197,
  #                          1, "(Intercept)", -26.61296, 17.35, "", -1.533888, 0.1266698, -60.82961, 7.603693, "TRUE",
  #                          "", "adverts", 0.08488483, 0.006923017, 0.5108462, 12.26125, 5.054937e-26, 0.07123166, 0.09853799,
  #                          "", "airplay", 3.367425, 0.2777708, 0.5119881, 12.12303, 1.326307e-25, 2.819622, 3.915228,
  #                          "", "attract", 11.08634, 2.437849, 0.1916834, 4.547588, 9.492121e-06, 6.278552, 15.89412)
  # )
})

# Chapter 3
test_that("Fields Book - Chapter 3 results match", {
  options <- initOptsLinReg()
  options$dependent <- "sales"
  options$covariates <- c("adverts", "airplay", "attract")
  options$modelTerms <- list(
    list(components=list("adverts"), name="model0", title = "Model 0"),
    list(components=list("adverts", "airplay", "attract"), name="model1", title = "Model 1")
  ) 
  options$residualVsFittedPlot <- TRUE
  options$partialResidualPlot <- TRUE
  options$residualHistogramPlot <- TRUE
  options$residualHistogramStandardizedPlot <- TRUE
  options$residualQqPlot <- TRUE
  options$residualCasewiseDiagnostic <- TRUE
  options$residualCasewiseDiagnosticType <- "outliersOutside"
  options$residualCasewiseDiagnosticZThreshold <- 2
  options$coefficientCi <- TRUE

  results <- jaspTools::runAnalysis("RegressionLinear", dataset = "Album Sales.csv", options)
  figure3 <- results[["state"]][["figures"]][[1]][["obj"]] # Residuals vs. Predicted
  jaspTools::expect_equal_plots(figure3, "field-residuals-predicted")
  figure4a <- results[["state"]][["figures"]][[4]][["obj"]] # Partial Plot Adverts
  jaspTools::expect_equal_plots(figure4a, "field-partial-adverts")
  figure4b <- results[["state"]][["figures"]][[5]][["obj"]] # Partial Plot Airplay
  jaspTools::expect_equal_plots(figure4b, "field-partial-airplay")
  figure4c <- results[["state"]][["figures"]][[6]][["obj"]] # Partial Plot Image
  jaspTools::expect_equal_plots(figure4c, "field-partial-attract")
  figure5a <- results[["state"]][["figures"]][[2]][["obj"]] # Standardized Residuals Histogram
  jaspTools::expect_equal_plots(figure5a, "field-residuals-histogram")
  figure5b <- results[["state"]][["figures"]][[3]][["obj"]] # Q-Q-Plot
  jaspTools::expect_equal_plots(figure5b, "field-qq")
  output1 <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_influenceTable"]][["data"]]
  jaspTools::expect_equal_tables(output1,
                      list(1, 2.177404, 330, 229.9203, 100.0797, 0.05870388,
                           2, -2.323083, 120, 228.949, -108.949, 0.01088943,
                           10, 2.130289, 300, 200.4662, 99.53375, 0.01775647,
                           47, -2.460996, 40, 154.9698, -114.9698, 0.02411519,
                           52, 2.099446, 190, 92.59734, 97.40266, 0.03315918,
                           55, -2.455913, 190, 304.1231, -114.1231, 0.0404159,
                           61, 2.104079, 300, 201.1897, 98.8103, 0.005948358,
                           68, -2.363549, 70, 180.4156, -110.4156, 0.02228898,
                           100, 2.095399, 250, 152.7133, 97.28666, 0.03136402,
                           164, -2.628814, 120, 241.324, -121.324, 0.07076588,
                           169, 3.093333, 360, 215.8675, 144.1325, 0.050867,
                           200, -2.088044, 110, 207.2061, -97.20606, 0.02513455)
  )

  options <- initOptsLinReg()
  options$dependent <- "sales"
  options$covariates <- c("adverts", "airplay", "attract")
  options$modelTerms <- list(
    list(components=list("adverts"), name="model0", title = "Model 0"),
    list(components=list("adverts", "airplay", "attract"), name="model1", title = "Model 1")
  ) 
  options$residualCasewiseDiagnostic <- TRUE
  options$residualCasewiseDiagnosticType <- "allCases"
  options$coefficientBootstrap <- TRUE
  options$coefficientBootstrapSamples <- 1000
  set.seed(1) # For Bootstrapping Unit Tests
  options$coefficientCi <- TRUE
  results <- jaspTools::runAnalysis("RegressionLinear", dataset = "Album Sales.csv", options)
  figure10 <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_influenceTable"]][["data"]]
  figure10 <- list(figure10[[1]]$cooksD, figure10[[2]]$cooksD, figure10[[3]]$cooksD, figure10[[4]]$cooksD,
                   figure10[[5]]$cooksD, figure10[[6]]$cooksD, figure10[[7]]$cooksD, figure10[[8]]$cooksD,
                   figure10[[9]]$cooksD, figure10[[10]]$cooksD, figure10[[11]]$cooksD, figure10[[12]]$cooksD,
                   figure10[[13]]$cooksD, figure10[[14]]$cooksD, figure10[[15]]$cooksD, figure10[[16]]$cooksD,
                   figure10[[17]]$cooksD, figure10[[18]]$cooksD, figure10[[19]]$cooksD, figure10[[20]]$cooksD,
                   figure10[[21]]$cooksD, figure10[[22]]$cooksD)
  jaspTools::expect_equal_tables(figure10,
                      list(0.05870388, 0.01088943, 0.01140066, 7.166478e-05, 0.0001025423,
                           0.001377347, 0.00594368, 0.0007230228, 0.0009490373, 0.01775647,
                           0.000822629, 0.01657069, 0.0003049181, 0.0003904011, 0.004041096,
                           0.002207233, 0.0002713418, 0.0001776211, 0.0004687904, 0.008795957,
                           0.0002810147, 0.0002291055)
  )
  # needs investigating
  # output2 <- results[["results"]][["bootstrap.regression"]][["data"]]
  # expect_equal_tables(output2,
  #                     list(0, "(Intercept)", -0.08265759, 134.1399, 8.228444, 118.874, 151.0766, "TRUE",
  #                          "", "adverts", 0.0002069432, 0.09612449, 0.008855503, 0.07704638, 0.1127528,
  #                          1, "(Intercept)", 1.258855, -26.61296, 15.91686, -54.69175, 8.180507, "TRUE",
  #                          "", "adverts", -6.543211e-05, 0.08488483, 0.007137828, 0.07036585, 0.09968162,
  #                          "", "airplay", 0.0128862, 3.367425, 0.3070985, 2.732326, 3.918479,
  #                          "", "attract", -0.2110928, 11.08634, 2.234141, 6.502079, 15.13605)
  # )

  options <- initOptsLinReg()
  options$dependent <- "spai"
  options$covariates <- c("tosca", "obq")
  options$modelTerms <- list(
    list(components="tosca", name="model0", title = "Model 0"),
    list(components="obq", name="model0", title = "Model 0")
  )
  options$modelTerms <- list(
    list(components=NULL, name="model0", title = "Model 0"),
    list(components=list("tosca", "obq"), name="model1", title = "Model 1")
  ) 
  options$residualVsFittedPlot <- TRUE
  options$partialResidualPlot <- TRUE
  options$residualQqPlot <- TRUE
  set.seed(1)
  results <- jaspTools::runAnalysis("RegressionLinear", dataset = "SocialAnxietyRegression.csv", options)
  figure11a <- results[["state"]][["figures"]][[1]][["obj"]] # Residuals vs. Predicted
  jaspTools::expect_equal_plots(figure11a, "socialAnxiety-residuals-vs.-predicted") # This command needs to be updated
  figure11b <- results[["state"]][["figures"]][[3]][["obj"]] # Partial Plot tosca
  jaspTools::expect_equal_plots(figure11b, "socialAnxiety-partialplot-tosca") # This command needs to be updated
  figure11c <- results[["state"]][["figures"]][[4]][["obj"]] # Partial Plot obq
  jaspTools::expect_equal_plots(figure11c, "socialAnxiety-partialplot-obq") # This command needs to be updated
  figure11d <- results[["state"]][["figures"]][[2]][["obj"]] # Q-Q-Plot
  jaspTools::expect_equal_plots(figure11d, "socialAnxiety-QQ-plot") # This command needs to be updated
})


# Chapter 4
test_that("Fields Book - Chapter 4 results match", {
  options <- initOptsLinReg()
  options$dependent <- "Happiness"
  options$covariates <- c("dummy1", "dummy2")
  options$modelTerms <- list(
    list(components=NULL, name="model0", title = "Model 0"),
    list(components=list("dummy1", "dummy2"), name="model1", title = "Model 1")
  ) 
  options$coefficientCi <- TRUE
  results <- jaspTools::runAnalysis("RegressionLinear", dataset = "Puppies Dummy.csv", options)
  output1a <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_anovaTable"]][["data"]]
  jaspTools::expect_equal_tables(output1a,
                      list("TRUE", 5.11864406779661, 10.0666666666667, 20.1333333333333,
                           "Regression", 2, "H<unicode>", 0.0246942895382226, "FALSE",
                           1.96666666666667, 23.6, "Residual", 12, "H<unicode>", "FALSE",
                           43.7333333333333, "Total", 14, "H<unicode>")
  )
  # needs investigating
  # output1b <- results[["results"]][["regression"]][["data"]]
  # expect_equal_tables(output1b,
  #                     list(1, "(Intercept)", 2.2, 0.6271629, "", 3.50786, 0.00431889, 0.8335294, 3.566471, "TRUE",
  #                          "", "dummy1", 2.8, 0.8869423, 0.7730207, 3.156913, 0.008268103, 0.8675187, 4.732481,
  #                          "", "dummy2", 1, 0.8869423, 0.2760788, 1.127469, 0.2815839, -0.9324813, 2.932481)
  # )
})


# Chapter 5
test_that("Fields Book - Chapter 5 results match", {
  options <- initOptsLinReg()
  options$dependent <- "Happiness"
  options$covariates <- c("Dummy1", "Dummy2")
  options$modelTerms <- list(
    list(components=NULL, name="model0", title = "Model 0"),
    list(components=list("Dummy1", "Dummy2"), name="model1", title = "Model 1")
  ) 
  options$coefficientCi <- TRUE
  results <- jaspTools::runAnalysis("RegressionLinear", dataset = "Puppies Contrast.csv", options)
  output1a <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_anovaTable"]][["data"]]
  jaspTools::expect_equal_tables(output1a,
                      list("TRUE", 5.11864406779661, 10.0666666666667, 20.1333333333333,
                           "Regression", 2, "H<unicode>", 0.0246942895382226, "FALSE",
                           1.96666666666667, 23.6, "Residual", 12, "H<unicode>", "FALSE",
                           43.7333333333333, "Total", 14, "H<unicode>")
  )
  # needs investigating
  # output1b <- results[["results"]][["regression"]][["data"]]
  # expect_equal_tables(output1b,
  #                     list(1, "(Intercept)", 3.466667, 0.3620927, "", 9.573976, 5.720565e-07, 2.677734, 4.255599, "TRUE",
  #                          "", "Dummy1", 0.6333333, 0.2560382, 0.5245497, 2.473589, 0.02930022, 0.07547404, 1.191193,
  #                          "", "Dummy2", 0.9, 0.4434712, 0.4303643, 2.029444, 0.06519221, -0.06624065, 1.866241)
  # )
})

test_that("VIF is correct when the model contains factors", {
# test issue reported in https://forum.cogsci.nl/discussion/comment/27675
# previously, we computed the variance inflation factor (vif) in an incorrect manner when categorical variables were in play.

  options <- initOptsLinReg()
  options$collinearityDiagnostic <- TRUE
  options$collinearityStatistic <- TRUE
  options$covariates <- c("contcor1", "contcor2", "contGamma")
  options$dependent <- "contNormal"
  options$factors <- c("contBinom", "facFive")
  options$modelTerms <- list(
    list(components=NULL, name="model0", title = "Model 0"),
    list(components=list("contcor1", "contcor2", "contGamma", "contBinom", "facFive",  
                         list("contBinom", "facFive"), 
                         list("contcor1", "facFive")),
         name="model1", title = "Model 1")
  ) 
  testData$contBinom <- as.factor(testData$contBinom)
  set.seed(1)
  results <- runAnalysis("RegressionLinear", testData, options)

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_coeffTable"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list("FALSE", 0.105841360919316, "H<unicode>", "(Intercept)", 0.0775993871437191,
         -1.78331595418434, -0.18874858754, "TRUE", 0.381077705677521,
         "H<unicode>", "(Intercept)", 0.501548251476625, -0.675005637319259,
         -0.257229599589016, "FALSE", 0.229415233303344, 2.23135132594493,
         "H<unicode>", "contcor1", 0.856898616177921, 0.0396717387702451,
         0.180885242815291, 0.448158919831471, 0.041497830181602, "FALSE",
         0.147439972438895, 1.42314119059934, "H<unicode>", "contcor2",
         0.712063663895108, -0.0518051388920147, -0.370352013821907,
         0.702670969405966, -0.0546046907105912, "FALSE", 0.0760326737019401,
         1.11997931274809, "H<unicode>", "contGamma", 0.870044727845177,
         -0.0180655487128054, -0.164108476295555, 0.892873634912333,
         -0.0124776062299025, "FALSE", 0.479235139503022, 2.28509437363427,
         "H<unicode>", "contBinom (1)", 0.802089668315079, -0.251447489341619,
         0.437618687235912, -0.120502472632315, "FALSE", 0.468673710760916,
         1.39741193512963, "H<unicode>", "facFive (2)", 0.693175018207069,
         -0.395926731022279, 0.715608601058093, -0.18556045021765, "FALSE",
         0.441277216318091, "H<unicode>", "facFive (3)", 0.199196838705615,
         1.29416779456092, 0.571086761832368, "FALSE", 0.455327789223056,
         "H<unicode>", "facFive (4)", 0.685753978744935, -0.406045691450261,
         -0.184883887011595, "FALSE", 0.438348369583355, "H<unicode>",
         "facFive (5)", 0.849342613590102, -0.190551094117348, -0.0835277614286641,
         "FALSE", 0.678318906929285, 1.6108410013664, "H<unicode>", "contBinom (1) <unicode><unicode><unicode> facFive (2)",
         0.601149755894428, 0.524758730058436, 0.620793733926407, 0.355953768174838,
         "FALSE", 0.68838076433972, "H<unicode>", "contBinom (1) <unicode><unicode><unicode> facFive (3)",
         0.635123142268019, -0.476284458187595, -0.327865059370306, "FALSE",
         0.668162941287466, "H<unicode>", "contBinom (1) <unicode><unicode><unicode> facFive (4)",
         0.617681282877862, 0.501022739247865, 0.334764827107757, "FALSE",
         0.76722436784419, "H<unicode>", "contBinom (1) <unicode><unicode><unicode> facFive (5)",
         0.561956805574511, 0.582279564339632, 0.446739070659064, "FALSE",
         0.40007211139567, 1.2579083938319, "H<unicode>", "contcor1 <unicode><unicode><unicode> facFive (2)",
         0.424016221112783, 0.803438605272872, 0.794970448486916, 0.32143337918831,
         "FALSE", 0.328695838682228, "H<unicode>", "contcor1 <unicode><unicode><unicode> facFive (3)",
         0.688152485067383, -0.402770662670618, -0.132389040763116, "FALSE",
         0.287054883639194, "H<unicode>", "contcor1 <unicode><unicode><unicode> facFive (4)",
         0.918577222183053, 0.102537696840895, 0.0294339466352942, "FALSE",
         0.350057215310513, "H<unicode>", "contcor1 <unicode><unicode><unicode> facFive (5)",
         0.00738001589526162, 2.74673560025106, 0.961514615418135))

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_collinearityTable"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(0.00449196686348337, "TRUE", 1, 0.00571243801751505, 0.00330931875916206,
         0.00378052571283837, 0.00384571521849041, 0.00186384004219264,
         0.0125009754753227, 6.03806519872408e-05, 0.000666087767585192,
         0.00147133293228095, 0.000239310815989242, 9.79677431623692e-05,
         3.80233650092307e-06, 1, 3.7138153322441, 0.00429532897717759,
         0.00393568696743585, 0.00474252333822674, 0.00229333066634962,
         "H<unicode>", 0.000150911573610796, "FALSE", 1.19888330411364,
         6.80713089909456e-05, 1.56397655274291e-05, 0.0049665142766426,
         0.00118768743000306, 4.94927245414001e-05, 0.000102304604657449,
         0.0239287022663734, 0.00651648421597881, 0.0250938202838133,
         0.017546025059876, 0.0109770737601213, 0.0448423227510986, 2,
         2.58384513633856, 1.28769013040331e-05, 0.00330009668083861,
         0.00355679670313216, 0.00209191695346411, "H<unicode>", 1.19565383685764e-05,
         "FALSE", 1.43151662446243, 0.00019819950696865, 0.0358687841692997,
         0.0129969193967221, 0.00624269743006397, 4.78853133019016e-05,
         0.000815242237489698, 0.000602803868252707, 0.0175257516242487,
         0.00981703885767484, 0.00305481430569416, 0.000984000403866895,
         0.000876134291565785, 3, 1.81228924437015, 0.0409265943109581,
         0.0128453193393263, 0.00854921708263547, 0.000233394455682235,
         "H<unicode>", 0.000466510857934275, "FALSE", 1.50377485464095,
         0.000345208310762601, 1.24635026720787e-05, 0.0137605771565279,
         0.0400750113794119, 0.00599541426569891, 6.01063659669367e-05,
         0.00308628360545295, 0.00371000337162385, 0.00120338283624414,
         2.14302412769005e-05, 0.0231962810364704, 0.00418767461235217,
         4, 1.64230822473588, 1.33710731560051e-05, 0.0160714036916076,
         0.0372398842666283, 0.0232852771074969, "H<unicode>", 8.44321920863671e-05,
         "FALSE", 1.56202433818173, 7.6002385826479e-05, 0.00481478573836742,
         0.0161830681575047, 0.000639954672358072, 0.0936527502527506,
         1.70875693403051e-05, 0.0032972658698283, 0.00039660015790756,
         0.00260513542172342, 0.0100357437850929, 0.00506584037830376,
         0.00202169021828179, 5, 1.52210534849705, 0.00480538979671842,
         0.0156885051999307, 6.92211160078503e-05, 0.0501746426653786,
         "H<unicode>", 0.000661918764325261, "FALSE", 1.75537046664085,
         0.00446694571638367, 0.00151672610057581, 0.00930004517092096,
         0.00153948292126512, 0.0403629308003015, 0.00956219884036096,
         0.000656020451354543, 0.0372175528617107, 0.0054467162670174,
         0.0377887845326724, 0.163701393071935, 0.0168061821669455, 6,
         1.20526551387999, 9.83046121501584e-05, 0.00463968980257777,
         0.00127824466281576, 0.0104219108935589, "H<unicode>", 4.835998444246e-05,
         "FALSE", 1.93918255306293, 0.00155383596377204, 0.00212785332110551,
         0.000143921499260893, 0.00376640530332463, 0.00250690087500955,
         0.00111608148685675, 5.62486279532585e-05, 0.326842301478478,
         0.025542934350284, 0.200369300540367, 0.000480145365955585,
         0.00306092208765049, 7, 0.98760416905077, 3.90758566748765e-06,
         0.000130387541673736, 0.000879727432836404, 0.00132601255259049,
         "H<unicode>", 0.000276745934307894, "FALSE", 2.01746980473893,
         0.000205379664394062, 0.00792289944792451, 0.00574251326809334,
         5.52087311384996e-06, 0.000452741640193997, 0.00404329389573302,
         0.000981884924527432, 0.194203085989231, 0.252433862658418,
         0.0424770701219298, 0.00900387221011966, 0.0208033885568837,
         8, 0.912443996481314, 0.00314258609263942, 0.0233813743196689,
         0.000204138687238012, 0.00242529037944073, "H<unicode>", 0.00744072291519576,
         "FALSE", 2.25953082086749, 0.0250508899254626, 0.00784841456352689,
         0.0380194308688003, 0.0343782015410773, 0.00919026536410998,
         0.107943401287479, 0.00151022098530686, 0.0554190899244452,
         0.00134547087060121, 0.0100809762345329, 0.08060477765883, 0.00457524199770884,
         9, 0.727417534453748, 0.00571136891635435, 0.00232328908467248,
         0.00185703491057558, 0.000277194190139293, "H<unicode>", 0.000205033822483049,
         "FALSE", 2.96709063198333, 0.00231586014787493, 0.00393239272373716,
         0.00247993979593613, 0.00343552439154568, 0.000233962268004041,
         0.0057038089830368, 0.0248638593536972, 0.0310276753227623,
         0.275901573130852, 0.0553056122866763, 0.00919145029194501,
         0.673004113702242, 10, 0.421850608715112, 0.014364128055905,
         0.000440657554652672, 0.0075726041413438, 0.000319388789073827,
         "H<unicode>", 0.00354029271673266, "FALSE", 3.11331015431941,
         0.00774246405085628, 0.00729266632892383, 0.201135883081112,
         0.0263021740912977, 0.0177899631918818, 0.104942857009563, 0.000589546187588995,
         4.42456094244631e-05, 0.00980713322328901, 0.00308084652304669,
         0.00373209849559993, 0.000448379071763017, 11, 0.383155909851674,
         0.00197169020914301, 0.323021463523386, 0.000387537679898297,
         0.000400312716991905, "H<unicode>", 0.000368177200389678, "FALSE",
         3.52148467640191, 0.000155445217091924, 0.0281813903440812,
         0.00158157055811355, 0.038853519432109, 0.367166569578754, 0.0267867818976713,
         0.00898618262843404, 0.00182160977977255, 0.00756188917595251,
         0.0203965155632414, 0.199277066738072, 0.000719190087861723,
         12, 0.299480603075679, 0.0193220406430508, 0.000737148090163242,
         0.0217734571950395, 0.343812520946708, "H<unicode>", 0.000829464665294348,
         "FALSE", 3.78348081185852, 0.050410785940705, 0.00104313923568573,
         0.135075333564519, 0.0384471073912495, 0.00148780668202332,
         0.175104377229794, 2.97220523912621e-06, 0.00585570304340318,
         0.00304396524817412, 0.0453309323661316, 0.00688923664971509,
         0.000149851154198664, 13, 0.259440177819091, 0.119370424155663,
         0.000773324250893733, 0.350793460001622, 0.0436524885587449,
         "H<unicode>", 0.000102963289558906, "FALSE", 3.82758447078491,
         0.00520565782615249, 0.218560886676004, 0.00984025086225854,
         0.170252268843305, 8.31099727119474e-05, 0.000525932668686379,
         0.00564844167386435, 0.000296373907260227, 0.0483724663548554,
         0.0028593066481767, 0.0224783262794197, 0.0604032959624838,
         14, 0.253495781816491, 0.304228665539697, 0.00356612541533721,
         0.144579968581697, 0.000463225121355087, "H<unicode>", 0.0780145792141872,
         "FALSE", 5.11899884124648, 0.0605413955900434, 0.0354134911668767,
         0.0310730491318464, 0.085774715830543, 0.070554679580587, 0.0759890676846888,
         0.421762655294923, 0.148197507792442, 0.194146629025121, 0.357778526870288,
         0.26134335796355, 0.0425029665537339, 15, 0.141726232470765,
         0.0255582381259842, 0.036897881239835, 0.000178931008236119,
         0.00897263190986998, "H<unicode>", 0.107540331302716, "FALSE",
         6.04225174158427, 0.137303340240153, 0.229360936172145, 0.0535850038278227,
         0.115288136011985, 0.0941327760403752, 0.311300545312021, 0.480277448771401,
         0.160107804781001, 0.118323179432952, 0.181310229711743, 0.183109164952259,
         0.124158057039896, 16, 0.101723823046663, 0.00469959895319038,
         0.0211612829558517, 5.21847606014747e-05, 0.0180876742449597,
         "H<unicode>", 0.795765632164884, "FALSE", 10.7675193947386,
         0.698648080187047, 0.412778211984386, 0.46033545367108, 0.429965877238857,
         0.294428911407562, 0.163485937451332, 0.0236890826338157, 0.010152122372725,
         0.0178834699307463, 0.0123245743932652, 0.0198679470006735,
         0.00143678740883328, 17, 0.0320323631529717, 0.451475486051242,
         0.531086364342148, 0.416285068431465, 0.491762787848195, "H<unicode>"
    ))
})
