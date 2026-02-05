context("Example: World Happiness")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("RegressionLinearBayesian results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "World Happiness.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("RegressionLinearBayesian", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["basreg"]][["collection"]][["basreg_modelComparisonTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 2.97655354135405, "jaspColumn1 + jaspColumn2 + jaspColumn3 + jaspColumn4 + jaspColumn5 + jaspColumn6",
     0.812424644406383, 0.331592022221154, 0.142857142857143, 0.178000847949936,
     18.4614770546394, "jaspColumn1 + jaspColumn2 + jaspColumn3 + jaspColumn4 + jaspColumn6",
     0.810730828246671, 0.31047794251184, 0.0238095238095238, 0.106215615309646,
     27.3338749354501, "jaspColumn1 + jaspColumn2 + jaspColumn3 + jaspColumn4",
     0.805632556508669, 0.208125093003496, 0.00952380952380953, 0.384696634964289,
     6.87815284329895, "jaspColumn1 + jaspColumn2 + jaspColumn3 + jaspColumn4 + jaspColumn5",
     0.808732926466749, 0.143659528090203, 0.0238095238095238, 8.85472478272706,
     0.260289514625058, "jaspColumn2 + jaspColumn3 + jaspColumn4 + jaspColumn6",
     0.793653784846203, 0.00249653550586531, 0.00952380952380953,
     18.8049157067436, 0.122659321266141, "jaspColumn1 + jaspColumn2 + jaspColumn4",
     0.784282171896045, 0.000881663144340079, 0.00714285714285715,
     35.0548284116373, 0.0656254459064509, "jaspColumn1 + jaspColumn2 + jaspColumn4 + jaspColumn5",
     0.789777623880815, 0.000630615975498684, 0.00952380952380953,
     26.4830884250786, 0.0870747467966026, "jaspColumn2 + jaspColumn3 + jaspColumn4",
     0.783288836089922, 0.000626044849639115, 0.00714285714285715,
     96.2758538750862, 0.0235487940048839, "jaspColumn2 + jaspColumn3 + jaspColumn4 + jaspColumn5 + jaspColumn6",
     0.793778711174808, 0.000574031128392415, 0.0238095238095238,
     62.3654858758189, 0.036877019344866, "jaspColumn1 + jaspColumn2 + jaspColumn4 + jaspColumn6",
     0.78813297045447, 0.0003544610372917, 0.00952380952380953))

})

