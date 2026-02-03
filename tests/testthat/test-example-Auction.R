context("Example: Auction")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("RegressionLinearBayesian (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "Auction.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("RegressionLinearBayesian", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["basreg"]][["collection"]][["basreg_modelComparisonTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 372991313.885908, "jaspColumn1 + jaspColumn2", 0.892712653281025,
     0.999999994637945, 0.333333333333333, 93316108.7197954, 2.67906584863826e-08,
     "jaspColumn1", 0.533240543967195, 5.35813166856695e-09, 0.166666666666667,
     227731184487.698, 1.09778552824214e-11, "jaspColumn2", 0.155741009930428,
     2.19557105647945e-12, 0.166666666666667, 578765446351.385, 3.45563129569605e-12,
     "Null model", 0, 1.72781564784504e-12, 0.333333333333333))

  plotName <- results[["results"]][["basreg"]][["collection"]][["basreg_postSumContainer"]][["collection"]][["basreg_postSumContainer_postSumPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-1_posterior-coefficients-with-95-credible-interval")

  table <- results[["results"]][["basreg"]][["collection"]][["basreg_postSumContainer"]][["collection"]][["basreg_postSumContainer_postSumTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, "Intercept", 1279.15542238794, 1327.15625, 0, 0, 1, 1, 23.5354308140833,
     1375.15707761205, 254881834333.037, "jaspColumn1", 10.7330040725468,
     12.5606969611098, 3.92341714672284e-12, 0.5, 0.999999999996077,
     0.5, 0.896141622303275, 14.3883898496728, 186572054.283367,
     "jaspColumn2", 66.9998695649758, 84.6326198879906, 5.35985955618656e-09,
     0.5, 0.99999999464014, 0.5, 8.64556708570363, 102.265370211005
    ))

})

