context("Example: Adam Sandler")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("CorrelationBayesian results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "Adam Sandler.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("CorrelationBayesian", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["corBayesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(5.02017487281891, -0.0286389027751801, "-", "jaspColumn1", "jaspColumn2"
    ))

  plotName <- results[["results"]][["pairsPlotCollection"]][["collection"]][["pairsPlotCollection_jaspColumn1-jaspColumn2"]][["collection"]][["pairsPlotCollection_jaspColumn1-jaspColumn2_bfRobustnessPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_bayes-factor-robustness-check")

  plotName <- results[["results"]][["pairsPlotCollection"]][["collection"]][["pairsPlotCollection_jaspColumn1-jaspColumn2"]][["collection"]][["pairsPlotCollection_jaspColumn1-jaspColumn2_bfSequentialPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_sequential-analysis")

  plotName <- results[["results"]][["pairsPlotCollection"]][["collection"]][["pairsPlotCollection_jaspColumn1-jaspColumn2"]][["collection"]][["pairsPlotCollection_jaspColumn1-jaspColumn2_priorPosteriorPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-3_prior-and-posterior")

  plotName <- results[["results"]][["pairsPlotCollection"]][["collection"]][["pairsPlotCollection_jaspColumn1-jaspColumn2"]][["collection"]][["pairsPlotCollection_jaspColumn1-jaspColumn2_scatterPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-4_scatterplot")

})

