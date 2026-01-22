context("Example: Titanic")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("RegressionLogistic results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "Titanic.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("RegressionLogistic", encoded$dataset, encoded$options, encodedDataset = TRUE)

  plotName <- results[["results"]][["diagnosticPlots"]][["collection"]][["diagnosticPlots_squaredPearson"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_squared-pearson-residuals-plot")

  plotName <- results[["results"]][["estimatesPlot"]][["collection"]][["estimatesPlot_jaspColumn1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_jaspcolumn1")

  plotName <- results[["results"]][["estimatesPlot"]][["collection"]][["estimatesPlot_jaspColumn3"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-3_jaspcolumn3")

  table <- results[["results"]][["estimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", -0.347366579504981, "M<unicode>", 0.706546275395035, "(Intercept)",
     2.54655183336127e-06, 0.0738391799970593, 1, 22.1310660444386,
     -4.70436669961416, "TRUE", 2.03370550244438, "M<unicode>", 7.64235271257249,
     "(Intercept)", 3.4251239494315e-11, 0.306880469523176, 1, 43.9175017652688,
     6.62702812467766, "FALSE", -0.0385040944204685, "M<unicode>",
     0.962227764966696, "jaspColumn1", 4.04523140840173e-09, 0.00654571811376704,
     1, 34.6018428729526, -5.88233311475577, "FALSE", -1.14582018402757,
     "M<unicode>", 0.317963022621508, "jaspColumn3 (2nd)", 1.97012457337813e-07,
     0.220259619094917, 1, 27.0622043280754, -5.20213459342176, "FALSE",
     -2.23150423113026, "M<unicode>", 0.107366804115719, "jaspColumn3 (3rd)",
     1.94041611183818e-22, 0.228992893374079, 1, 94.9623427372085,
     -9.7448623765145))

  table <- results[["results"]][["modelSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1027.57254724533, 1032.20058862151, 1025.57254724533, 755, 0,
     "M<unicode>", 0, 917.916717627966, 936.428883132685, 0.141856463570706,
     115.655829617361, 909.916717627966, 752, 0.112771963258973,
     "M<unicode>", 0.191062953887039, 0, 0.147637783148129))

  table <- results[["results"]][["perfDiag"]][["collection"]][["perfDiag_confusionMatrix"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 79.0067720090293, 350, 93, 1, 50.4792332268371, 155, 158, "Overall % Correct",
     67.1957671957672, "", ""))

  table <- results[["results"]][["perfDiag"]][["collection"]][["perfDiag_performanceMetrics"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Sensitivity", 0.504792332268371, "Specificity", 0.790067720090293
    ))

})

