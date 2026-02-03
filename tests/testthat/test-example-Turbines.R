context("Example: Turbines")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("GeneralizedLinearModel results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "Turbines.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("GeneralizedLinearModel", encoded$dataset, encoded$options, encodedDataset = TRUE)

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
    list(9, 2.10698284422105, 1, -1.36906660365315, 11, -1.08991121387782
    ))

  plotName <- results[["results"]][["diagnosticsContainer"]][["collection"]][["diagnosticsContainer_partialResidualPlot"]][["collection"]][["diagnosticsContainer_partialResidualPlot_jaspColumn1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-4_partial-residual-plot-for-jaspcolumn1")

  plotName <- results[["results"]][["diagnosticsContainer"]][["collection"]][["diagnosticsContainer_quantileResidualVsPredictorPlot"]][["collection"]][["diagnosticsContainer_quantileResidualVsPredictorPlot_jaspColumn1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-5_standardized-quantile-residuals-vs-jaspcolumn1")

  table <- results[["results"]][["estimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-4.70447922924669, -3.21909139466423, -3.92359655506097, "(Intercept)",
     3.02549037349438e-25, 0.377958944698631, -10.3810125678848,
     0.000783381003863431, 0.00123187871784827, 0.000999237231007308,
     "jaspColumn1", 2.06501535779309e-18, 0.000114150499713778, 8.75368249383753
    ))

  table <- results[["results"]][["modelFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(9, 10.3314656594169, "Deviance", 0.324323585924635, 9, 9.25083866566519,
     "Pearson", 0.414449329827485))

  table <- results[["results"]][["modelSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(150.14676873699, 150.544664009789, "", 112.670049156685, 10, "H<unicode>",
     "", 49.8081852397221, 50.6039757853188, 102.338583497268, 10.3314656594169,
     9, "H<unicode>", 4.67987874608065e-24))

})

