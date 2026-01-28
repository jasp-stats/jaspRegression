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
    list(1, 2.97655354135293, "jaspColumn1 + jaspColumn2 + jaspColumn3 + jaspColumn4 + jaspColumn5 + jaspColumn6",
     0.812424644406383, 0.33159202222107, 0.142857142857143, 0.178000847949939,
     18.4614770546323, "jaspColumn1 + jaspColumn2 + jaspColumn3 + jaspColumn4 + jaspColumn6",
     0.810730828246671, 0.310477942511757, 0.0238095238095238, 0.106215615309644,
     27.3338749354419, "jaspColumn1 + jaspColumn2 + jaspColumn3 + jaspColumn4",
     0.805632556508669, 0.208125093003447, 0.00952380952380953, 0.384696634963639,
     6.87815284331051, "jaspColumn1 + jaspColumn2 + jaspColumn3 + jaspColumn4 + jaspColumn5",
     0.808732926466753, 0.14365952809041, 0.0238095238095238, 8.85472478271209,
     0.260289514625434, "jaspColumn2 + jaspColumn3 + jaspColumn4 + jaspColumn6",
     0.793653784846207, 0.0024965355058689, 0.00952380952380953,
     18.8049157067139, 0.122659321266304, "jaspColumn1 + jaspColumn2 + jaspColumn4",
     0.784282171896049, 0.000881663144341247, 0.00714285714285715,
     35.0548284115227, 0.065625445906649, "jaspColumn1 + jaspColumn2 + jaspColumn4 + jaspColumn5",
     0.789777623880824, 0.000630615975500586, 0.00952380952380953,
     26.4830884250361, 0.0870747467967205, "jaspColumn2 + jaspColumn3 + jaspColumn4",
     0.783288836089925, 0.000626044849639962, 0.00714285714285715,
     96.2758538749275, 0.0235487940049168, "jaspColumn2 + jaspColumn3 + jaspColumn4 + jaspColumn5 + jaspColumn6",
     0.793778711174812, 0.000574031128393217, 0.0238095238095238,
     62.3654858757196, 0.0368770193449154, "jaspColumn1 + jaspColumn2 + jaspColumn4 + jaspColumn6",
     0.788132970454474, 0.000354461037292174, 0.00952380952380953
    ))

})

