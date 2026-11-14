context("Example: Presidents' Height")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("CorrelationBayesian (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "Presidents' Height.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Run analysis
  set.seed(1)
  results <- jaspTools::runAnalysis("CorrelationBayesian", dataset, opts)

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

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "Presidents' Height.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Run analysis
  set.seed(1)
  results <- jaspTools::runAnalysis("CorrelationBayesian", dataset, opts)

  table <- results[["results"]][["corBayesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1.35371131181883, 0.393092363401853, "-", "Height Ratio", "Popular Vote"
    ))

  plotName <- results[["results"]][["pairsPlotCollection"]][["collection"]][["pairsPlotCollection_Height Ratio-Popular Vote"]][["collection"]][["pairsPlotCollection_Height Ratio-Popular Vote_priorPosteriorPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-1_prior-and-posterior")

})

