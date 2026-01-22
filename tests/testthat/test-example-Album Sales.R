context("Example: Album Sales")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("RegressionLinear (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "Album Sales.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("RegressionLinear", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_anovaTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", 99.5868714961988, 433687.832532257, 433687.832532257,
     "Regression", 1, "M<unicode>", 2.94197985216586e-19, "FALSE",
     4354.86953266536, 862264.167467742, "Residual", 198, "M<unicode>",
     "FALSE", 1295952, "Total", 199, "M<unicode>"))

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_coeffTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("FALSE", 5.70627794978487, "M<unicode>", "(Intercept)", 1.59908972195556e-84,
     33.8574464300821, 193.2, "TRUE", 7.53657467947199, "M<unicode>",
     "(Intercept)", 5.9678171979745e-43, 17.7985283125294, 134.139937812074,
     "FALSE", 0.00963236621523019, "M<unicode>", "jaspColumn1", 2.94197985216575e-19,
     0.578487741981689, 9.97932219623151, 0.0961244859738772))

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 0, 80.698956672563, 0, "M<unicode>", 0.578487741981689, 0.334648067623073,
     65.9914352978124, 0.331287704328241, "M<unicode>"))

})

test_that("Correlation (analysis 3) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "Album Sales.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[3]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("Correlation", encoded$dataset, encoded$options, encodedDataset = TRUE)

  plotName <- results[["results"]][["corrPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-1_correlation-plot")

  table <- results[["results"]][["mainTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("<unicode>", "<unicode>", 0.578487741981689, 1.47098992608298e-19,
     0.598918797598853, 3.71256583393774e-21, 0.326111052237613,
     1.22366825028454e-06, "jaspColumn1", "", "", "<unicode>", "<unicode>",
     0.101882810327344, 0.0755655792503952, 0.0807515070094317, 0.127832275464997,
     "jaspColumn2", "", "", "", "", "<unicode>", "<unicode>", 0.181988633025724,
     0.00495218610474086, "jaspColumn3", "", "", "", "", "", "",
     "<unicode>", "<unicode>", "jaspColumn4"))

})

test_that("RegressionLinear (analysis 4) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "Album Sales.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[4]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("RegressionLinear", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_anovaTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("FALSE", 99.5868714961988, 433687.832532257, 433687.832532257,
     "Regression", 1, "M<unicode>", 2.94197985216586e-19, "FALSE",
     4354.86953266536, 862264.167467742, "Residual", 198, "M<unicode>",
     "FALSE", 1295952, "Total", 199, "M<unicode>", "TRUE", 129.498273390974,
     287125.806089999, 861377.418269998, "Regression", 3, "M<unicode>",
     2.87553505648738e-46, "FALSE", 2217.2172537245, 434574.581730001,
     "Residual", 196, "M<unicode>", "FALSE", 1295952, "Total", 199,
     "M<unicode>"))

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_coeffTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("FALSE", 7.53657467947199, 119.277680821137, "M<unicode>", "(Intercept)",
     5.9678171979745e-43, 17.7985283125294, 134.139937812074, 149.002194803011,
     "FALSE", 0.00963236621523019, 0.0771292914445232, "M<unicode>",
     "jaspColumn1", 2.94197985216575e-19, 0.578487741981689, 9.97932219623151,
     0.0961244859738772, 0.115119680503231, "TRUE", 17.3500005649351,
     -60.8296096716467, "M<unicode>", "(Intercept)", 0.126669773576492,
     -1.53388804006522, -26.6129583616786, 7.60369294828949, "FALSE",
     0.00692301687199933, 0.0712316582019458, "M<unicode>", "jaspColumn1",
     5.05493680123644e-26, 0.510846225434073, 12.2612477656671, 0.0848848251534775,
     0.0985379921050092, "FALSE", 0.277770831682515, 2.81962186317387,
     "M<unicode>", "jaspColumn2", 1.32630717054857e-25, 0.511988143597586,
     12.1230337617277, 3.36742517051031, 3.91522847784675, "FALSE",
     2.43784926487601, 6.27855217709138, "M<unicode>", "jaspColumn3",
     9.49212095293172e-06, 0.191683427305029, 4.54758846836075, 11.0863352045519,
     15.8941182320123))

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_collinearityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.107365142721572, "FALSE", 1, 1, 1.78526971455686, 0.107365142721572,
     "M<unicode>", 0.892634857278428, "FALSE", 2.88340248305523,
     2, 0.214730285443144, 0.892634857278428, "M<unicode>", 0.00277331978429811,
     "TRUE", 1, 1, 3.56209116007091, 0.0225857350059735, 0.0112171605460822,
     0.00292553596994339, "M<unicode>", 0.00638420233464002, "FALSE",
     3.40061065062144, 2, 0.308028717932371, 0.959884547749367, 0.0534836169252784,
     0.00762912419680943, "M<unicode>", 0.0536863667854121, "FALSE",
     5.70369260905287, 3, 0.109494626328104, 0.0152175030783235,
     0.932404779245674, 0.0685406415888725, "M<unicode>", 0.93715611109565,
     "FALSE", 13.2187952880243, 4, 0.0203854956686162, 0.00231221416633612,
     0.00289444328296513, 0.920904698244375, "M<unicode>"))

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_descriptivesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(200, 80.698956672563, 5.70627794978487, 193.2, "jaspColumn4",
     200, 485.655207961017, 34.3410090867798, 614.412255, "jaspColumn1",
     200, 12.2695848979433, 0.867590668367979, 27.5, "jaspColumn2",
     200, 1.39528999525341, 0.0986619017365434, 6.77, "jaspColumn3"
    ))

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_influenceTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 0.058703882060172, 330, 229.920254782559, 100.079745217441,
     2.17740408238375, 2, 0.010889432250004, 120, 228.948991725879,
     -108.948991725879, -2.32308277865479, 10, 0.017756471647924,
     300, 200.466248756041, 99.5337512439587, 2.13028877629469, 47,
     0.024115187915313, 40, 154.969819283836, -114.969819283836,
     -2.46099615091923, 52, 0.0331591773716575, 190, 92.5973385351762,
     97.4026614648238, 2.0994462133587, 55, 0.040415896570736, 190,
     304.123081395714, -114.123081395714, -2.45591293513707, 61,
     0.00594835802830878, 300, 201.189704978232, 98.8102950217684,
     2.10407875290071, 68, 0.0222889831193746, 70, 180.415641506173,
     -110.415641506173, -2.36354940981169, 100, 0.0313640206351772,
     250, 152.713339076214, 97.2866609237863, 2.09539916076742, 164,
     0.0707658817516239, 120, 241.32404875998, -121.32404875998,
     -2.62881409071654, 169, 0.0508669997203079, 360, 215.867537706138,
     144.132462293862, 3.09333296653134, 200, 0.0251345530959372,
     110, 207.206059703631, -97.2060597036308, -2.08804417375657
    ))

  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_residualsQQPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-4_figure-1_q-q-plot-standardized-residuals")

  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_residualsVsHistPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-4_figure-2_standardized-residuals-histogram")

  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_residualsVsPredPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-4_figure-3_residuals-vs-predicted")

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.578487741981689, 0.334648067623073, 0.334648067623073, 65.9914352978124,
     0.331287704328241, 1, 198, "M<unicode>", 2.9419798521659e-19,
     0.815271541864744, 0.664667686974517, 0.330019619351443, 47.0873364475471,
     0.659535049530249, 2, 196, "M<unicode>", 6.87939491037878e-30
    ))

})

