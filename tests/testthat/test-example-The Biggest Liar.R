context("Example: The Biggest Liar")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("Correlation (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "The Biggest Liar.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Run analysis
  set.seed(1)
  results <- jaspTools::runAnalysis("Correlation", dataset, opts)

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

