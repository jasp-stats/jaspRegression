context("Library: Adam Sandler")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/library/.

test_that("CorrelationBayesian results match", {

  # Run the R syntax of analysis 1 of the JASP example file on the data of the file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Adam Sandler.jasp")
  set.seed(1)
  results <- jaspTools::runAnalysisSyntax(jaspFile = jaspFile, jaspRegression::CorrelationBayesian(
          data = NULL,
          version = "0.96.5",
          alternative = "greater",
          bayesFactorType = "BF01",
          bfRobustnessPlot = TRUE,
          bfSequentialPlot = TRUE,
          bfSequentialPlotAdditionalInfo = FALSE,
          plotHeight = 240,
          plotWidth = 320,
          priorPosteriorPlot = TRUE,
          variablePairs = list("Freshness", "Box Office ($M)"),
          variables = ~ Freshness + `Box Office ($M)`))

  table <- results[["results"]][["corBayesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(5.02017487281891, -0.0286389027751801, "-", "Freshness", "Box Office ($M)"
    ))

  plotName <- results[["results"]][["pairsPlotCollection"]][["collection"]][["pairsPlotCollection_Box Office ($M)-Freshness"]][["collection"]][["pairsPlotCollection_Box Office ($M)-Freshness_bfRobustnessPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_bayes-factor-robustness-check")

  plotName <- results[["results"]][["pairsPlotCollection"]][["collection"]][["pairsPlotCollection_Box Office ($M)-Freshness"]][["collection"]][["pairsPlotCollection_Box Office ($M)-Freshness_bfSequentialPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_sequential-analysis")

  plotName <- results[["results"]][["pairsPlotCollection"]][["collection"]][["pairsPlotCollection_Box Office ($M)-Freshness"]][["collection"]][["pairsPlotCollection_Box Office ($M)-Freshness_priorPosteriorPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-3_prior-and-posterior")

  plotName <- results[["results"]][["pairsPlotCollection"]][["collection"]][["pairsPlotCollection_Box Office ($M)-Freshness"]][["collection"]][["pairsPlotCollection_Box Office ($M)-Freshness_scatterPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-4_scatterplot")

})

