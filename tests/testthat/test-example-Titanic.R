context("Example: Titanic")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("RegressionLogistic results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "Titanic.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Run analysis
  set.seed(1)
  results <- jaspTools::runAnalysis("RegressionLogistic", dataset, opts)

  plotName <- results[["results"]][["diagnosticPlots"]][["collection"]][["diagnosticPlots_squaredPearson"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_squared-pearson-residuals-plot")

  plotName <- results[["results"]][["estimatesPlot"]][["collection"]][["estimatesPlot_Age"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_age")

  plotName <- results[["results"]][["estimatesPlot"]][["collection"]][["estimatesPlot_PClass"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-3_pclass")

  table <- results[["results"]][["estimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", -0.347366579504981, "M<unicode>", 0.706546275395035, "(Intercept)",
     2.54655183336097e-06, 0.0738391799970589, 1, 22.1310660444387,
     -4.70436669961418, "TRUE", 2.03370550244437, "M<unicode>", 7.6423527125724,
     "(Intercept)", 3.42512394943197e-11, 0.306880469523176, 1, 43.9175017652685,
     6.62702812467764, "FALSE", -0.0385040944204683, "M<unicode>",
     0.962227764966696, "Age", 4.04523140840232e-09, 0.00654571811376703,
     1, 34.601842872952, -5.88233311475575, "FALSE", -1.14582018402756,
     "M<unicode>", 0.31796302262151, "PClass (2nd)", 1.97012457337834e-07,
     0.220259619094917, 1, 27.0622043280752, -5.20213459342174, "FALSE",
     -2.23150423113025, "M<unicode>", 0.10736680411572, "PClass (3rd)",
     1.94041611183849e-22, 0.228992893374078, 1, 94.9623427372075,
     -9.74486237651448))

  table <- results[["results"]][["modelSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1027.57254724532, 1032.2005886215, 1025.57254724532, 755, 0, "M<unicode>",
     0, 917.916717627967, 936.428883132685, 0.141856463570693, 115.65582961735,
     3, 909.916717627967, 752, 0.112771963258963, "M<unicode>", 0.191062953887022,
     0, 0.147637783148128))

  table <- results[["results"]][["perfDiag"]][["collection"]][["perfDiag_confusionMatrix"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 79.0067720090293, 350, 93, 1, 50.4792332268371, 155, 158, "Overall % Correct",
     67.1957671957672, "", ""))

  table <- results[["results"]][["perfDiag"]][["collection"]][["perfDiag_performanceMetrics"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Sensitivity", 0.504792332268371, "Specificity", 0.790067720090293
    ))

})

