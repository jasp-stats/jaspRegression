context("Library: Presidents' Height")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/library/.

test_that("CorrelationBayesian (analysis 1) results match", {

  # Run the R syntax of analysis 1 of the JASP example file on the data of the file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Presidents' Height.jasp")
  set.seed(1)
  results <- jaspTools::runAnalysisSyntax(jaspFile = jaspFile, jaspRegression::CorrelationBayesian(
          data = NULL,
          version = "0.96.5",
          bfRobustnessPlotAdditionalInfo = FALSE,
          bfSequentialPlotAdditionalInfo = FALSE,
          plotHeight = 240,
          plotWidth = 320,
          priorPosteriorPlot = TRUE,
          variablePairs = list("Height Ratio", "Popular Vote"),
          variables = ~ `Height Ratio` + `Popular Vote`))

  table <- results[["results"]][["corBayesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(6.33194472055065, 0.393092363401853, "-", "Height Ratio", "Popular Vote"
    ))

  plotName <- results[["results"]][["pairsPlotCollection"]][["collection"]][["pairsPlotCollection_Height Ratio-Popular Vote"]][["collection"]][["pairsPlotCollection_Height Ratio-Popular Vote_priorPosteriorPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_prior-and-posterior")

  plotName <- results[["results"]][["pairsPlotCollection"]][["collection"]][["pairsPlotCollection_Height Ratio-Popular Vote"]][["collection"]][["pairsPlotCollection_Height Ratio-Popular Vote_scatterPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_scatterplot")

})

test_that("CorrelationBayesian (analysis 2) results match", {

  # Run the R syntax of analysis 2 of the JASP example file on the data of the file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Presidents' Height.jasp")
  set.seed(1)
  results <- jaspTools::runAnalysisSyntax(jaspFile = jaspFile, jaspRegression::CorrelationBayesian(
          data = NULL,
          version = "0.96.5",
          bfRobustnessPlotAdditionalInfo = FALSE,
          bfSequentialPlotAdditionalInfo = FALSE,
          plotHeight = 240,
          plotWidth = 320,
          priorPosteriorPlot = TRUE,
          priorWidth = 0.005,
          scatterPlot = FALSE,
          variablePairs = list("Height Ratio", "Popular Vote"),
          variables = ~ `Height Ratio` + `Popular Vote`))

  table <- results[["results"]][["corBayesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1.35371131181883, 0.393092363401853, "-", "Height Ratio", "Popular Vote"
    ))

  plotName <- results[["results"]][["pairsPlotCollection"]][["collection"]][["pairsPlotCollection_Height Ratio-Popular Vote"]][["collection"]][["pairsPlotCollection_Height Ratio-Popular Vote_priorPosteriorPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-1_prior-and-posterior")

})

