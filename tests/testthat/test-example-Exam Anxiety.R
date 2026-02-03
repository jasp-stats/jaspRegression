context("Example: Exam Anxiety")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("Correlation results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "Exam Anxiety.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("Correlation", encoded$dataset, encoded$options, encodedDataset = TRUE)

  plotName <- results[["results"]][["corrPlot"]][["collection"]][["corrPlot_jaspColumn1_jaspColumn2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_jaspcolumn1-vs-jaspcolumn2")

  plotName <- results[["results"]][["corrPlot"]][["collection"]][["corrPlot_jaspColumn1_jaspColumn3"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_jaspcolumn1-vs-jaspcolumn3")

  plotName <- results[["results"]][["corrPlot"]][["collection"]][["corrPlot_jaspColumn2_jaspColumn3"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-3_jaspcolumn2-vs-jaspcolumn3")

  table <- results[["results"]][["mainTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.396720696926757, 0.220093785299448, 3.34345114805374e-05, 0.548160228665234,
     "-", "jaspColumn1", "jaspColumn2", -0.709249255109869, -0.793816825756736,
     5.10867485061462e-17, -0.597773293353828, "-", "jaspColumn1",
     "jaspColumn3", -0.440993411608789, -0.584624415209784, 3.12787277300793e-06,
     -0.270559099702507, "-", "jaspColumn2", "jaspColumn3"))

})

