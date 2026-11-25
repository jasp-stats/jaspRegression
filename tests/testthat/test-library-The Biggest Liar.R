context("Library: The Biggest Liar")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/library/.

test_that("Correlation (analysis 2) results match", {

  # Run the R syntax of analysis 2 of the JASP example file on the data of the file
  jaspFile <- testthat::test_path("jaspfiles", "library", "The Biggest Liar.jasp")
  set.seed(1)
  results <- jaspTools::runAnalysisSyntax(jaspFile = jaspFile, jaspRegression::Correlation(
          data = NULL,
          version = "0.96.5",
          kendallsTauB = TRUE,
          pairwiseDisplay = FALSE,
          pearson = FALSE,
          scatterPlot = TRUE,
          spearman = TRUE,
          variables = ~ Position + Creativity))

  plotName <- results[["results"]][["corrPlot"]][["collection"]][["corrPlot_Position_Creativity"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-1_position-vs-creativity")

  table <- results[["results"]][["mainTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.300241308065175, 0.00125880227934681, -0.373218381287678, 0.00172041688956587,
     "<unicode>", "<unicode>", "<unicode>", "<unicode>", "Position",
     "<unicode>", "<unicode>", "<unicode>", "<unicode>", "", "",
     "", "", "Creativity"))

})

