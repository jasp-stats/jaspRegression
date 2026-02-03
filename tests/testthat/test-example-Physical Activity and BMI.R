context("Example: Physical Activity and BMI")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("RegressionLinear (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "Physical Activity and BMI.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("RegressionLinear", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_anovaTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", 17.0964419058049, 228.37718667551, 228.37718667551, "Regression",
     1, "M<unicode>", 7.50303223656677e-05, "FALSE", 13.3581705441275,
     1309.10071332449, "Residual", 98, "M<unicode>", "FALSE", 1537.4779,
     "Total", 99, "M<unicode>"))

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_coeffTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("FALSE", 0.394082222359494, 23.1570553740754, "M<unicode>", "(Intercept)",
     3.58969265693091e-80, 60.7462063542721, 23.939, 24.7209446259246,
     "TRUE", 1.41197828710779, 26.7762221827865, "M<unicode>", "(Intercept)",
     5.70690457996434e-38, 20.9480892235324, 29.5782471400245, 32.3802720972626,
     "FALSE", 0.158336131923101, -0.968898666442348, "M<unicode>",
     "jaspColumn1", 7.50303223656677e-05, -0.385409058932744, -4.13478438443952,
     -0.654685765768193, -0.340472865094038))

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 0, 3.94082222359494, 0, "M<unicode>", 0.385409058932744, 0.148540142707423,
     3.65488310950261, 0.139851776816683, "M<unicode>"))

})

