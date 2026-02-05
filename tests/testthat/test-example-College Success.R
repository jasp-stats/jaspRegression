context("Example: College Success")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("Correlation (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "College Success.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("Correlation", encoded$dataset, encoded$options, encodedDataset = TRUE)

  plotName <- results[["results"]][["corrPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-1_correlation-plot")

  table <- results[["results"]][["mainTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("<unicode>", "<unicode>", "<unicode>", "<unicode>", 0.436498848113675,
     7.77354971314224e-12, 0.462679542529614, 2.77720968073502e-13,
     0.32942533474857, 4.55155779602865e-07, 0.394768349790076, 9.01627929268268e-10,
     0.289001251129858, 1.10605357902049e-05, 0.316444239638638,
     1.33536474409288e-06, 0.251714289292978, 0.000140248840146243,
     0.272364811193021, 3.59677533775691e-05, 0.114490457352839,
     0.0873444086720017, 0.154732971781111, 0.0205121453587086, "jaspColumn1",
     "", "", "", "", "<unicode>", "<unicode>", "<unicode>", "<unicode>",
     0.575686462532693, 3.60291088969295e-21, 0.586825467201838,
     4.03375259555243e-22, 0.446886524039091, 2.14403567474047e-12,
     0.482867911209179, 1.73950308925378e-14, 0.453513938554233,
     9.2108935120871e-13, 0.446178669766789, 2.3439915130473e-12,
     0.221120294875802, 0.000861455287029316, 0.217657022068208,
     0.00104262587520609, "jaspColumn2", "", "", "", "", "", "",
     "", "", "<unicode>", "<unicode>", "<unicode>", "<unicode>",
     0.579374573923577, 1.7612140823946e-21, 0.597768140833473, 4.31753812483039e-23,
     0.240479307301479, 0.000280710860529159, 0.236812566591759,
     0.000349627095568203, 0.261697540575234, 7.36665258064162e-05,
     0.271426201472893, 3.83563374061189e-05, "jaspColumn3", "",
     "", "", "", "", "", "", "", "", "", "", "", "<unicode>", "<unicode>",
     "<unicode>", "<unicode>", 0.108284905960014, 0.106018844244152,
     0.109507634352714, 0.102109738033041, 0.243714598935605, 0.000230626324471842,
     0.27405287373889, 3.20209979691565e-05, "jaspColumn4", "", "",
     "", "", "", "", "", "", "", "", "", "", "", "", "", "", "<unicode>",
     "<unicode>", "<unicode>", "<unicode>", 0.463941882345148, 2.34785170280405e-13,
     0.402870804891071, 3.76843749680409e-10, "jaspColumn5", "",
     "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
     "", "", "", "", "<unicode>", "<unicode>", "<unicode>", "<unicode>",
     "jaspColumn6"))

})

test_that("RegressionLinear (analysis 3) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "College Success.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[3]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("RegressionLinear", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_anovaTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", 18.8605939811381, 9.23744377485109, 27.7123313245533,
     "Regression", 3, "M<unicode>", 6.35876966578504e-11, "FALSE",
     0.489774806885148, 107.750457514732, "Residual", 220, "M<unicode>",
     "FALSE", 135.462788839286, "Total", 223, "M<unicode>"))

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_coeffTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("FALSE", 0.0520755140308844, "M<unicode>", "(Intercept)", 3.1984114465288e-124,
     50.603882905944, 2.63522321428571, "TRUE", 0.294243238369676,
     "M<unicode>", "(Intercept)", 0.0462181675004632, 2.00472446075233,
     0.589876617370669, "FALSE", 0.0354921389555153, "M<unicode>",
     "jaspColumn1", 3.6783558477195e-06, 0.354424100897158, 4.74940781331416,
     0.168566642066556, "FALSE", 0.0375588804528074, "M<unicode>",
     "jaspColumn2", 0.361902428552296, 0.0748335520044262, 0.91364725094587,
     0.034315567874312, "FALSE", 0.0386958544303089, "M<unicode>",
     "jaspColumn3", 0.245059890369688, 0.0872572393364943, 1.16554667712417,
     0.0451018245497272))

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_collinearityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.00159845292909155, "TRUE", 1, 1, 3.9453399845121, 0.00152890062572292,
     0.00143809029794921, 0.00136684285710626, "M<unicode>", 0.637807161414834,
     "FALSE", 13.5088985958251, 2, 0.0216194454841061, 0.101835228670563,
     0.357904987782276, 0.0216679977755401, "M<unicode>", 0.0443095016682574,
     "FALSE", 14.2403790126589, 3, 0.0194554525455711, 0.633668282505902,
     0.0978710132603883, 0.411003749648518, "M<unicode>", 0.316284883987817,
     "FALSE", 17.0416059156517, 4, 0.0135851174582266, 0.262967588197811,
     0.542785908659387, 0.565961409718836, "M<unicode>"))

  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_residualsQQPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-1_q-q-plot-standardized-residuals")

  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_residualsVsCovContainer"]][["collection"]][["modelContainer_residualsVsCovContainer_jaspColumn1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-2_residuals-vs-jaspcolumn1")

  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_residualsVsCovContainer"]][["collection"]][["modelContainer_residualsVsCovContainer_jaspColumn2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-3_residuals-vs-jaspcolumn2")

  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_residualsVsCovContainer"]][["collection"]][["modelContainer_residualsVsCovContainer_jaspColumn3"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-4_residuals-vs-jaspcolumn3")

  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_residualsVsHistPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-5_residuals-histogram")

  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_residualsVsPredPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-6_residuals-vs-predicted")

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 0, 0.779394926974834, 0, "M<unicode>", 0.452299934825555, 0.204575231043201,
     0.69983912928983, 0.193728529648336, "M<unicode>"))

})

test_that("RegressionLinear (analysis 4) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "College Success.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[4]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("RegressionLinear", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_anovaTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("FALSE", 18.8605939811381, 9.23744377485109, 27.7123313245533,
     "Regression", 3, "M<unicode>", 6.35876966578504e-11, "FALSE",
     0.489774806885148, 107.750457514732, "Residual", 220, "M<unicode>",
     "FALSE", 135.462788839286, "Total", 223, "M<unicode>", "TRUE",
     11.6913773234137, 5.72872897825734, 28.6436448912867, "Regression",
     5, "M<unicode>", 5.05825265181081e-10, "FALSE", 0.489996073155959,
     106.819143947999, "Residual", 218, "M<unicode>", "FALSE", 135.462788839286,
     "Total", 223, "M<unicode>"))

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_coeffTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("FALSE", 0.294243238369676, "M<unicode>", "(Intercept)", 0.0462181675004632,
     2.00472446075233, 0.589876617370669, "FALSE", 0.0354921389555153,
     "M<unicode>", "jaspColumn1", 3.6783558477195e-06, 0.354424100897158,
     4.74940781331416, 0.168566642066556, "FALSE", 0.0375588804528074,
     "M<unicode>", "jaspColumn2", 0.361902428552296, 0.0748335520044262,
     0.91364725094587, 0.034315567874312, "FALSE", 0.0386958544303089,
     "M<unicode>", "jaspColumn3", 0.245059890369688, 0.0872572393364943,
     1.16554667712417, 0.0451018245497272, "TRUE", 0.399996431126465,
     "M<unicode>", "(Intercept)", 0.414932071338614, 0.816804135293862,
     0.326718739046883, "FALSE", 0.0392609736479122, "M<unicode>",
     "jaspColumn1", 0.00025562792414473, 0.306894197756009, 3.71771420797734,
     0.145961079549867, "FALSE", 0.0377984123420551, "M<unicode>",
     "jaspColumn2", 0.343206794844332, 0.0783003980334905, 0.949916085526176,
     0.0359053198910693, "FALSE", 0.0395686907374339, "M<unicode>",
     "jaspColumn3", 0.163719232330563, 0.10697301150704, 1.39738212788037,
     0.0552925812601156, "FALSE", 0.000685657268712742, "M<unicode>",
     "jaspColumn4", 0.170175780811658, 0.104603912296396, 1.3761868977175,
     0.000943592549527239, "FALSE", 0.000591892694084862, "M<unicode>",
     "jaspColumn5", 0.491518300679486, -0.0484621203098972, -0.689059888795698,
     -0.000407849513965101))

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_collinearityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.00159845292909155, "FALSE", 1, 1, 3.9453399845121, 0.00152890062572292,
     0.00143809029794921, 0.00136684285710626, "M<unicode>", 0.637807161414834,
     "FALSE", 13.5088985958251, 2, 0.0216194454841061, 0.101835228670563,
     0.357904987782276, 0.0216679977755401, "M<unicode>", 0.0443095016682574,
     "FALSE", 14.2403790126589, 3, 0.0194554525455711, 0.633668282505902,
     0.0978710132603883, 0.411003749648518, "M<unicode>", 0.316284883987817,
     "FALSE", 17.0416059156517, 4, 0.0135851174582266, 0.262967588197811,
     0.542785908659387, 0.565961409718836, "M<unicode>", 0.000388045711650771,
     "TRUE", 1, 1, 5.90167469838739, 0.000557185516597868, 0.000631279070966488,
     0.000581563454052863, 0.000364263701308553, 0.000667412093281148,
     "M<unicode>", 0.0185131430359409, "FALSE", 12.4676571196172,
     2, 0.0379669375076991, 0.0389180363772827, 0.162398438923656,
     0.0761758415470425, 0.0530739514222912, 0.188536579235057, "M<unicode>",
     0.000593587331575786, "FALSE", 16.0821523271622, 3, 0.0228184912005348,
     0.376393091656946, 0.00978333717979085, 0.247316629016724, 0.0774636135588146,
     0.148298939781424, "M<unicode>", 0.191056824540608, "FALSE",
     18.6797199296607, 4, 0.0169135412339674, 0.000123434536557944,
     0.408414236422926, 0.267973375489163, 0.00553070044771628, 0.320196923395952,
     "M<unicode>", 0.260030883534617, "FALSE", 21.3558203868742,
     5, 0.0129402524279801, 0.359086645415053, 0.413346291988767,
     0.186437185095149, 0.0375530932379864, 0.229127130328058, "M<unicode>",
     0.529417515845608, "FALSE", 27.709915720048, 6, 0.00768607924243065,
     0.224921606497562, 0.00542641641389256, 0.221515405397868, 0.826014377631883,
     0.113173015166227, "M<unicode>"))

  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_residualsQQPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-4_figure-1_q-q-plot-standardized-residuals")

  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_residualsVsCovContainer"]][["collection"]][["modelContainer_residualsVsCovContainer_jaspColumn1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-4_figure-2_residuals-vs-jaspcolumn1")

  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_residualsVsCovContainer"]][["collection"]][["modelContainer_residualsVsCovContainer_jaspColumn2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-4_figure-3_residuals-vs-jaspcolumn2")

  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_residualsVsCovContainer"]][["collection"]][["modelContainer_residualsVsCovContainer_jaspColumn3"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-4_figure-4_residuals-vs-jaspcolumn3")

  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_residualsVsCovContainer"]][["collection"]][["modelContainer_residualsVsCovContainer_jaspColumn4"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-4_figure-5_residuals-vs-jaspcolumn4")

  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_residualsVsCovContainer"]][["collection"]][["modelContainer_residualsVsCovContainer_jaspColumn5"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-4_figure-6_residuals-vs-jaspcolumn5")

  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_residualsVsHistPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-4_figure-7_residuals-histogram")

  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_residualsVsPredPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-4_figure-8_residuals-vs-predicted")

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.452299934825555, 0.204575231043201, 0.204575231043201, 0.69983912928983,
     0.193728529648336, 3, 220, "M<unicode>", 6.35876966578504e-11,
     0.459837234172533, 0.211450281931445, 0.00687505088824328, 0.69999719510578,
     0.19336427922345, 2, 218, "M<unicode>", 0.388210041729659))

})

