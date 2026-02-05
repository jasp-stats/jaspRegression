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

  plotName <- results[["results"]][["heatmaps"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-11_pearson-s-r-heatmap")

  table <- results[["results"]][["mainTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.350078559661139, -0.424730517332571, 7.3229619483706e-16, -0.270698020821203,
     "-", "jaspColumn1", "jaspColumn2", -0.0103826467347444, -0.0979840238070771,
     0.816857869597485, 0.0773783908706577, "-", "jaspColumn1", "jaspColumn3",
     -0.134322225812043, -0.219428213531435, 0.0026153282451028,
     -0.0471874494565636, "-", "jaspColumn1", "jaspColumn4", -0.368006436206105,
     -0.441451087941555, 1.75902170749649e-17, -0.289663492413263,
     "-", "jaspColumn1", "jaspColumn5", 0.267130892389201, 0.18374449471465,
     1.28782907848919e-09, 0.346700077546044, "-", "jaspColumn2",
     "jaspColumn3", 0.0545473493953726, -0.033302533897992, 0.223387556959248,
     0.141560810754256, "-", "jaspColumn2", "jaspColumn4", 0.0645917390961748,
     -0.0232304277256849, 0.149244102980625, 0.151424644297581, "-",
     "jaspColumn2", "jaspColumn5", 0.159208473526591, 0.0725304901911447,
     0.000351675992676603, 0.243499543445293, "-", "jaspColumn3",
     "jaspColumn4", -0.0134488495998729, -0.10102030021429, 0.76418760028676,
     0.0743293974929718, "-", "jaspColumn3", "jaspColumn5", 0.158670001224872,
     0.0719809468027763, 0.000368493644268167, 0.242979800569512,
     "-", "jaspColumn4", "jaspColumn5"))

})

