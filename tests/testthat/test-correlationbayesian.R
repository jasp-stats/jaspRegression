context("Bayesian Correlation")

# does not test
# - bftype (01, 10)
# - missing value exclusion
# - errors whilst plotting

test_that("Bayesian Correlation Table results match", {
  options <- jaspTools::analysisOptions("CorrelationBayesian")
  options$variables <- c("contcor1", "contcor2")
  options$kendall <- TRUE
  options$supportCorrelationFlagged <- TRUE
  options$ci <- TRUE
  options$priorWidth <- 1.5
  set.seed(1)
  results <- jaspTools::runAnalysis("CorrelationBayesian", "test.csv", options)
  table <- results[["results"]][["corBayesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list("TRUE", "<unicode>", "Pearson's r", "1. contcor1", "FALSE", "<unicode>",
                           "BF<unicode><unicode>", "", "FALSE", "<unicode>", "Upper 95% CI",
                           "", "FALSE", "<unicode>", "Lower 95% CI", "", "FALSE", "<unicode>",
                           "Kendall's tau", "", "FALSE", "<unicode>", "BF<unicode><unicode>",
                           "", "FALSE", "<unicode>", "Upper 95% CI", "", "FALSE", "<unicode>",
                           "Lower 95% CI", "", 1, "TRUE", 0.657010063712354, "<unicode>",
                           "Pearson's r", "2. contcor2", "FALSE", 71191291327.7135, "<unicode>",
                           "BF<unicode><unicode>", "", "FALSE", 0.753487301516098, "<unicode>",
                           "Upper 95% CI", "", "FALSE", 0.524567068817432, "<unicode>",
                           "Lower 95% CI", "", 1, "FALSE", 0.503030303030303, "<unicode>",
                           "Kendall's tau", "", "FALSE", 78878934747.8062, "<unicode>",
                           "BF<unicode><unicode>", "", "FALSE", 0.621810905452726, "<unicode>",
                           "Upper 95% CI", "", "FALSE", 0.36168084042021, "<unicode>",
                           "Lower 95% CI", ""))
})

# Note(Alexander): The difference in the Kendall's version is quite small. The differences in ci is due to me using
# a larger number of samplers.
#
# test_that("Main table results match", {
#   options <- jaspTools::analysisOptions("CorrelationBayesian")
#   options$variables <- c("contcor1", "contcor2")
#   options$kendallsTauB <- TRUE
#   options$reportBayesFactors <- TRUE
#   options$supportCorrelationFlagged <- TRUE
#   options$credibleInterval <- TRUE
#   options$priorWidth <- 1.5
#   results <- jaspTools::runAnalysis("CorrelationBayesian", "test.csv", options)
#   table <- results[["results"]][["correlations"]][["data"]]
#   jaspTools::expect_equal_tables(table,
#     list("Pearson's r", "<unicode>", "", "BF<unicode><unicode>", "<unicode>",
#          "", "Upper 95% CI", "<unicode>", "", "Lower 95% CI", "<unicode>",
#          "", "Kendall's tau", "<unicode>", "", "BF<unicode><unicode>",
#          "<unicode>", "", "Upper 95% CI", "<unicode>", "", "Lower 95% CI",
#          "<unicode>", "", "contcor1", "Pearson's r", 0.657010063712354,
#          "<unicode>", "BF<unicode><unicode>", 71191291327.7147, "<unicode>",
#          "Upper 95% CI", 0.753487301516087, "<unicode>", "Lower 95% CI",
#          0.524567068817447, "<unicode>", "Kendall's tau", 0.503030303030303,
#          "<unicode>", "BF<unicode><unicode>", 78878934747.8062, "<unicode>",
#          "Upper 95% CI", 0.62124248496994, "<unicode>", "Lower 95% CI",
#          0.360721442885771, "<unicode>", "contcor2", "***", "***")
#   )
# })


test_that("Bayesian Correlation Matrix Plot matches", {
  options <- jaspTools::analysisOptions("CorrelationBayesian")
  options$variables <- c("contcor1", "contcor2")
  options$matrixPlot <- TRUE
  options$matrixPlotDensity <- TRUE
  options$matrixPlotPosterior <- TRUE
  options$priorWidth <- 1.0
  set.seed(1)
  results <- jaspTools::runAnalysis("CorrelationBayesian", "test.csv", options)
  plotName <- results[["results"]][["matrixPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "bayesian-correlation-matrix-plot")
})

test_that("Analysis handles errors", {
  options <- jaspTools::analysisOptions("CorrelationBayesian")

  options$variables <- c("contNormal", "debInf")
  results <- jaspTools::runAnalysis("CorrelationBayesian", "test.csv", options)
  notes <- unlist(results[["results"]][["corBayesTable"]][["footnotes"]])
  expect_true(any(grepl("infinity", notes, ignore.case=TRUE)), label = "Inf check")

  options$variables <- c("contNormal", "debSame")
  results <- jaspTools::runAnalysis("CorrelationBayesian", "test.csv", options)
  notes <- unlist(results[["results"]][["corBayesTable"]][["footnotes"]])
  expect_true(any(grepl("variance", notes, ignore.case=TRUE)), label = "No variance check")

  options$variables <- c("contNormal", "debMiss99")
  results <- jaspTools::runAnalysis("CorrelationBayesian", "test.csv", options)
  notes <- unlist(results[["results"]][["corBayesTable"]][["footnotes"]])
  expect_true(any(grepl("observations", notes, ignore.case=TRUE)), label = "Too few obs check")
})


# Bayesian correlation pairs
test_that("Bayesian Pearson Correlation PAIRWISE table results match", {
  options <- jaspTools::analysisOptions("CorrelationBayesian")
  options$variables <- list("contcor1", "contcor2")
  options$pairwiseDisplay <- TRUE
  options$ci <- TRUE
  options$priorWidth <- 2
  set.seed(1)
  results <- jaspTools::runAnalysis("CorrelationBayesian", "test.csv", options)
  table <- results[["results"]][["corBayesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(64323420095.6647, 0.526024457777766, 0.657010063712354, 0.754530668995199,
                           "-", "contcor1", "contcor2"))
})

# Pairs plot
options <- jaspTools::analysisOptions("CorrelationBayesian")
options$variables <- list("contcor1", "contcor2")
options$variablePairs <- list(c("contcor1", "contcor2"))
options$priorPosteriorPlot <- TRUE
options$bfRobustnessPlot <- TRUE
options$bfSequentialPlot <- TRUE
options$priorWidth <- 1.0
set.seed(1)
results <- jaspTools::runAnalysis("CorrelationBayesian", "test.csv", options)

test_that("Bayes Factor Robustness Check plot matches", {
  plotName <- results[["results"]][["pairsPlotCollection"]][["collection"]][["pairsPlotCollection_contcor1-contcor2"]][["collection"]][["pairsPlotCollection_contcor1-contcor2_bfRobustnessPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "bayes-factor-robustness-check")
})

test_that("Sequential Analysis plot matches", {
  plotName <- results[["results"]][["pairsPlotCollection"]][["collection"]][["pairsPlotCollection_contcor1-contcor2"]][["collection"]][["pairsPlotCollection_contcor1-contcor2_bfSequentialPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "sequential-analysis")
})

test_that("Prior and Posterior plot matches", {
  plotName <- results[["results"]][["pairsPlotCollection"]][["collection"]][["pairsPlotCollection_contcor1-contcor2"]][["collection"]][["pairsPlotCollection_contcor1-contcor2_priorPosteriorPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "prior-and-posterior")
})

test_that("Scatterplot matches", {
  plotName <- results[["results"]][["pairsPlotCollection"]][["collection"]][["pairsPlotCollection_contcor1-contcor2"]][["collection"]][["pairsPlotCollection_contcor1-contcor2_scatterPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "scatterplot")
})

options <- jaspTools::analysisOptions("CorrelationBayesian")
options$variables <- list("facFive", "contBinom")
options$pearson <- FALSE
options$kendall <- TRUE
options$pairwiseDisplay <- TRUE
options$matrixPlot <- TRUE
options$pairsMethod <- "kendall"
options$variablePairs <- list(c("facFive", "contBinom"))
options$priorWidth <- 1
set.seed(1)
results <- jaspTools::runAnalysis("CorrelationBayesian", "test.csv", options)


test_that("Bayesian Kendall's Tau Correlations table results match", {
	table <- results[["results"]][["corBayesTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.309897604337613, -0.0896993701844904, "-", "facFive", "contBinom"
			))
})

test_that("Bayesian Correlation Matrix Plot with ranks matches", {
	plotName <- results[["results"]][["matrixPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "bayesian-correlation-matrix-plot-with-ranks")
})

test_that("Scatterplot with ranks matches", {
	plotName <- results[["results"]][["pairsPlotCollection"]][["collection"]][["pairsPlotCollection_contBinom-facFive"]][["collection"]][["pairsPlotCollection_contBinom-facFive_scatterPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "scatterplot-with-ranks")
})

# TODO(Alexander): Solve in  and perhaps in
#   1. .drawPosteriorPlotCorBayes
#   2. .getPosteriorPlotValuesCorBayes
#   3. bstats::computeCorPosteriorLine
#
# test_that("Too peaked posterior is plotted with meaningfull error message", {
#   options <- jaspTools::analysisOptions("CorrelationBayesian")
#   options$variables <- c("x", "y")
#   options$variablePairs <- list(
#     c("x", "y")
#   )
#   options$alternative <- "greater"
#   options$priorPosteriorPlot <- TRUE
#   options$priorPosteriorPlotAdditionalEstimationInfo <- TRUE
#   options$priorPosteriorPlotAdditionalTestingInfo <- TRUE
#   options$priorWidth <- "1"
#
#   set.seed(1)
#   data <- data.frame(x = rnorm(10))
#   data$y <- data$x + rnorm(10, sd = 0.01)
#
#   results <- jaspTools::runAnalysis("CorrelationBayesian", data, options)
#   error <- results[["results"]][["pairsPlotCollection"]][["collection"]][["pairsPlotCollection_x-y"]][["collection"]][["pairsPlotCollection_x-y_priorPosteriorPlot"]][["error"]][["errorMessage"]]
#
#   testthat::expect_equal(error, "Posterior is too peaked")
# })
