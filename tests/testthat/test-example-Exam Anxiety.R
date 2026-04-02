context("Example: Exam Anxiety")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("Correlation (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "Exam Anxiety.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
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

test_that("RegressionLinear (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "Exam Anxiety.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("RegressionLinear", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_anovaTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", 38.1027905463632, 5378.84863725272, 16136.5459117582,
     "Regression", 3, "M<unicode>", 1.85904462451184e-16, "FALSE",
     141.166790151702, 13975.5122250185, "Residual", 99, "M<unicode>",
     "FALSE", 30112.0581367767, "Total", 102, "M<unicode>"))

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_coeffTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("FALSE", 1.69297880821846, "M<unicode>", "(Intercept)", 4.57176741289056e-68,
     43.9129359103706, 74.3436699029126, "TRUE", 3.08003619145141,
     "M<unicode>", "(Intercept)", 2.88430771792394e-52, 30.601001298531,
     94.252191494127, "FALSE", 0.0708967734390473, "M<unicode>",
     "jaspColumn1", 1.70387692669837e-13, -0.63940953722996, -8.53352242728872,
     -0.604999206164517, "FALSE", 0.0494490896883354, "M<unicode>",
     "jaspColumn2", 0.0138418805036516, -0.187086477371529, -2.50596225491754,
     -0.123917552299001, "FALSE", 2.352185614503, "M<unicode>", "jaspColumn4 (Male)",
     0.457234643528689, -0.746333451613223, -1.755514808507))

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 0, 0, 17.1818583466082, 0, 0, 102, "M<unicode>", "", 0.732040432385989,
     0.535883194647866, 0.535883194647866, 11.8813631436676, 0.521819049031135,
     3, 99, "M<unicode>", 1.85904462451185e-16))

})

