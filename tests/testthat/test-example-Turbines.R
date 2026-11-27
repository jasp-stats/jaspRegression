context("Example: Turbines")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("GeneralizedLinearModel results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "Turbines.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Run analysis
  set.seed(1)
  results <- jaspTools::runAnalysis("GeneralizedLinearModel", dataset, opts)

  plotName <- results[["results"]][["diagnosticsContainer"]][["collection"]][["diagnosticsContainer_glmPlotResQQ"]][["collection"]][["diagnosticsContainer_glmPlotResQQ_quantileResidualQqPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_q-q-plot-standardized-quantile-residuals")

  plotName <- results[["results"]][["diagnosticsContainer"]][["collection"]][["diagnosticsContainer_glmPlotResVsFitted"]][["collection"]][["diagnosticsContainer_glmPlotResVsFitted_quantileResidualVsFittedPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_standardized-quantile-residuals-vs-fitted-values")

  plotName <- results[["results"]][["diagnosticsContainer"]][["collection"]][["diagnosticsContainer_glmPlotZVsEta"]][["collection"]][["diagnosticsContainer_glmPlotZVsEta_glmPlotZVsEta"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-3_plot-working-responses-vs-linear-predictor")

  table <- results[["results"]][["diagnosticsContainer"]][["collection"]][["diagnosticsContainer_outlierTables"]][["collection"]][["diagnosticsContainer_outlierTables_quantileResidualOutlierTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(9, 2.10698284432175, 1, -1.36906660374493, 11, -1.08991121365178
    ))

  plotName <- results[["results"]][["diagnosticsContainer"]][["collection"]][["diagnosticsContainer_partialResidualPlot"]][["collection"]][["diagnosticsContainer_partialResidualPlot_Hours"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-4_partial-residual-plot-for-hours")

  plotName <- results[["results"]][["diagnosticsContainer"]][["collection"]][["diagnosticsContainer_quantileResidualVsPredictorPlot"]][["collection"]][["diagnosticsContainer_quantileResidualVsPredictorPlot_Hours"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-5_standardized-quantile-residuals-vs-hours")

  table <- results[["results"]][["estimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-4.70447922904707, -3.21909139451491, -3.92359655488897, "(Intercept)",
     3.02549037679962e-25, 0.37795894468586, -10.3810125677805, 0.000783381003814545,
     0.00123187871778689, 0.000999237230952839, "Hours", 2.0650153620726e-18,
     0.000114150499710604, 8.75368249360376))

  table <- results[["results"]][["modelFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(9, 10.3314656582448, "Deviance", 0.32432358601475, 9, 9.25083866431235,
     "Pearson", 0.41444932994877))

  table <- results[["results"]][["modelSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(150.14676873699, 150.544664009789, "", "", 112.670049147247, 10,
     "H<unicode>", "", 49.8081852397221, 50.6039757853188, 102.338583489002,
     1, 10.3314656582448, 9, "H<unicode>", 4.67987876560886e-24
    ))

})

