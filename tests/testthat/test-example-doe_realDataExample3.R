context("Example: doe_realDataExample3")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("RegressionLinear results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "doe_realDataExample3.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("RegressionLinear", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_anovaTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", 16.1121412590273, 12795.3472222222, 140748.819444445,
     "Regression", 11, "M<unicode>", 1.00856773498578e-11, "FALSE",
     794.143187830685, 33354.0138888888, "Residual", 42, "M<unicode>",
     "FALSE", 174102.833333333, "Total", 53, "M<unicode>"))

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_coeffTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("FALSE", 7.79952410644257, "M<unicode>", "(Intercept)", 0.696805040576116,
     0.391761793906324, 3.05555555555556, "TRUE", 317.740663657973,
     "M<unicode>", "(Intercept)", 5.35667146639695e-07, 5.90969370549714,
     1877.75000000001, "FALSE", 4.97335413689561, "M<unicode>", "jaspColumn1",
     8.0901345129412e-11, -12.3107782954472, -8.60711882197071, -42.8062500000001,
     "FALSE", 12.0661890325388, "M<unicode>", "jaspColumn2", 4.21510076910479e-09,
     6.40014278249701, 7.37736384094561, 89.0166666666662, "FALSE",
     77.7451737454572, "M<unicode>", "jaspColumn4 (2)", 0.110358949232861,
     1.63104086654593, 126.805555555555, "FALSE", 77.7451737454572,
     "M<unicode>", "jaspColumn4 (3)", 0.33406770238368, -0.977195349398317,
     -75.9722222222249, "FALSE", 0.0203375574654828, "M<unicode>",
     "jaspColumn1<unicode>", 6.50260075783039e-11, "", 8.67647620088209,
     0.176458333333334, "FALSE", 0.325400919447725, "M<unicode>",
     "jaspColumn2<unicode>", 9.31517935208946e-12, "", -9.30134638772245,
     -3.02666666666666, "FALSE", 0.057523299186456, "M<unicode>",
     "jaspColumn1 <unicode><unicode><unicode> jaspColumn2", 0.468482798511151,
     0.40893861262055, 0.731587616296594, 0.0420833333333352, "FALSE",
     0.575232991864559, "M<unicode>", "jaspColumn1 <unicode><unicode><unicode> jaspColumn4 (2)",
     0.459778100131382, -0.746074499787576, -0.429166666666662, "FALSE",
     0.575232991864559, "M<unicode>", "jaspColumn1 <unicode><unicode><unicode> jaspColumn4 (3)",
     0.150863491786209, 1.46317523259315, 0.841666666666681, "FALSE",
     2.30093196745824, "M<unicode>", "jaspColumn2 <unicode><unicode><unicode> jaspColumn4 (2)",
     0.0170440606067324, -2.4845005187101, -5.71666666666664, "FALSE",
     2.30093196745824, "M<unicode>", "jaspColumn2 <unicode><unicode><unicode> jaspColumn4 (3)",
     0.370402483216455, -0.905430218188793, -2.08333333333328))

  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_responseOptimizer"]][["collection"]][["modelContainer_responseOptimizer_optimizationPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_optimization-plot")

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_responseOptimizer"]][["collection"]][["modelContainer_responseOptimizer_optimizationPlotTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 535.5067, 40, 1, 1))

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_responseOptimizer"]][["collection"]][["modelContainer_responseOptimizer_settingsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Minimize", "", "jaspColumn3", 0.5, 1, 1))

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_responseOptimizer"]][["collection"]][["modelContainer_responseOptimizer_solutionTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 12, 120, 15, 1))

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 0, 0, 57.3145628919636, 0, 0, 53, "M<unicode>", "", 0.899123732303933,
     0.808423485992155, 0.808423485992155, 28.1805462656543, 0.758248684704387,
     11, 42, "M<unicode>", 1.00856773498578e-11))

})

