context("Example: Big Five Personality Traits")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("Correlation results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "Big Five Personality Traits.jasp")
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

  plotName <- results[["results"]][["corrPlot"]][["collection"]][["corrPlot_jaspColumn1_jaspColumn4"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-3_jaspcolumn1-vs-jaspcolumn4")

  plotName <- results[["results"]][["corrPlot"]][["collection"]][["corrPlot_jaspColumn1_jaspColumn5"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-4_jaspcolumn1-vs-jaspcolumn5")

  plotName <- results[["results"]][["corrPlot"]][["collection"]][["corrPlot_jaspColumn2_jaspColumn3"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-5_jaspcolumn2-vs-jaspcolumn3")

  plotName <- results[["results"]][["corrPlot"]][["collection"]][["corrPlot_jaspColumn2_jaspColumn4"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-6_jaspcolumn2-vs-jaspcolumn4")

  plotName <- results[["results"]][["corrPlot"]][["collection"]][["corrPlot_jaspColumn2_jaspColumn5"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-7_jaspcolumn2-vs-jaspcolumn5")

  plotName <- results[["results"]][["corrPlot"]][["collection"]][["corrPlot_jaspColumn3_jaspColumn4"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-8_jaspcolumn3-vs-jaspcolumn4")

  plotName <- results[["results"]][["corrPlot"]][["collection"]][["corrPlot_jaspColumn3_jaspColumn5"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-9_jaspcolumn3-vs-jaspcolumn5")

  plotName <- results[["results"]][["corrPlot"]][["collection"]][["corrPlot_jaspColumn4_jaspColumn5"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-10_jaspcolumn4-vs-jaspcolumn5")

  table <- results[["results"]][["mainTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.350078559661138, -0.42473051733257, 7.32296194837149e-16, -0.270698020821202,
     "-", "jaspColumn1", "jaspColumn2", -0.0103826467347444, -0.0979840238070771,
     0.816857869597485, 0.0773783908706577, "-", "jaspColumn1", "jaspColumn3",
     -0.134322225812043, -0.219428213531435, 0.00261532824510278,
     -0.0471874494565636, "-", "jaspColumn1", "jaspColumn4", -0.368006436206105,
     -0.441451087941555, 1.75902170749661e-17, -0.289663492413262,
     "-", "jaspColumn1", "jaspColumn5", 0.267130892389201, 0.18374449471465,
     1.28782907848922e-09, 0.346700077546044, "-", "jaspColumn2",
     "jaspColumn3", 0.0545473493953725, -0.033302533897992, 0.223387556959249,
     0.141560810754256, "-", "jaspColumn2", "jaspColumn4", 0.0645917390961747,
     -0.023230427725685, 0.149244102980625, 0.151424644297581, "-",
     "jaspColumn2", "jaspColumn5", 0.159208473526591, 0.0725304901911449,
     0.000351675992676597, 0.243499543445293, "-", "jaspColumn3",
     "jaspColumn4", -0.013448849599873, -0.101020300214291, 0.764187600286759,
     0.0743293974929718, "-", "jaspColumn3", "jaspColumn5", 0.158670001224873,
     0.0719809468027765, 0.000368493644268159, 0.242979800569513,
     "-", "jaspColumn4", "jaspColumn5"))

})

