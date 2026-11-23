context("Library: Exam Anxiety")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/library/.

test_that("Correlation (analysis 1) results match", {

  # Run the R syntax of analysis 1 of the JASP example file on the data of the file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Exam Anxiety.jasp")
  set.seed(1)
  results <- jaspTools::runAnalysisSyntax(jaspFile = jaspFile, jaspRegression::Correlation(
          data = NULL,
          version = "0.96.5",
          ci = TRUE,
          scatterPlot = TRUE,
          scatterPlotDensity = TRUE,
          scatterPlotStatistic = TRUE,
          variables = ~ Revise + Exam + Anxiety))

  plotName <- results[["results"]][["corrPlot"]][["collection"]][["corrPlot_Exam_Anxiety"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_exam-vs-anxiety")

  plotName <- results[["results"]][["corrPlot"]][["collection"]][["corrPlot_Revise_Anxiety"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_revise-vs-anxiety")

  plotName <- results[["results"]][["corrPlot"]][["collection"]][["corrPlot_Revise_Exam"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-3_revise-vs-exam")

  table <- results[["results"]][["mainTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.396720696926757, 0.220093785299448, 3.34345114805374e-05, 0.548160228665234,
     "-", "Revise", "Exam", -0.709249255109869, -0.793816825756736,
     5.10867485061462e-17, -0.597773293353828, "-", "Revise", "Anxiety",
     -0.440993411608789, -0.584624415209784, 3.12787277300793e-06,
     -0.270559099702507, "-", "Exam", "Anxiety"))

})

test_that("RegressionLinear (analysis 2) results match", {

  # Run the R syntax of analysis 2 of the JASP example file on the data of the file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Exam Anxiety.jasp")
  set.seed(1)
  results <- jaspTools::runAnalysisSyntax(jaspFile = jaspFile, jaspRegression::RegressionLinear(
          data = NULL,
          version = "0.96.5",
          covarianceRatio = TRUE,
          covariates = ~ Revise + Exam,
          dependent = ~ Anxiety,
          dfbetas = TRUE,
          dffits = TRUE,
          equationTable = TRUE,
          factors = ~ Gender,
          leverage = TRUE,
          mahalanobis = TRUE,
          modelTerms = list(list(components = list(types = list(), value = list()), name = "model0", title = "Model 0"), list(components = list(types = list("scale", "scale", "nominal"), value = list("Revise", "Exam", "Gender")), name = "model1", title = "Model 1")),
          optimizationPlotCustomParameterValues = list(optionKey = "variable", types = list("scale", "scale", "nominal"), value = list(list(variable = "Revise"), list(variable = "Exam"), list(variable = "Gender"))),
          qqPlotCi = TRUE,
          residualCasewiseDiagnostic = TRUE,
          residualCasewiseDiagnosticZThreshold = 1.5,
          residualStatistic = TRUE))

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_anovaTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", 38.1027905463632, 5378.84863725272, 16136.5459117582,
     "Regression", 3, "M<unicode>", 1.85904462451184e-16, "FALSE",
     141.166790151702, 13975.5122250185, "Residual", 99, "M<unicode>",
     "FALSE", 30112.0581367767, "Total", 102, "M<unicode>"))

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_coeffTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("FALSE", 1.69297880821846, "M<unicode>", "(Intercept)", 4.57176741289056e-68,
     43.9129359103706, 74.3436699029126, "TRUE", 3.08003619145141,
     "M<unicode>", "(Intercept)", 2.88430771792394e-52, 30.601001298531,
     94.252191494127, "FALSE", 0.0708967734390473, "M<unicode>",
     "Revise", 1.70387692669837e-13, -0.63940953722996, -8.53352242728872,
     -0.604999206164517, "FALSE", 0.0494490896883354, "M<unicode>",
     "Exam", 0.0138418805036516, -0.187086477371529, -2.50596225491754,
     -0.123917552299001, "FALSE", 2.352185614503, "M<unicode>", "Gender (Male)",
     0.457234643528689, -0.746333451613223, -1.755514808507))

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_equationTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Anxiety = 74.344", "M<unicode>", "Anxiety = 94.252 <unicode> 0.605 Revise <unicode> 0.124 Exam <unicode> 1.756 Gender (Male)",
     "M<unicode>"))

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_influenceTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 0, 0, 0, 0.237576705230233, 0.0301836623467328, 0.227141716247882,
     -1.03949834683376, 24, 0.338306654934628, 0.841853042255204,
     0.056, -1.21068229775292, 0.137012897698042, 12.9850243030644,
     32.2796784693975, -32.2236784693975, -2.91948799000202, 0.0314897993750843,
     0.118022155675512, -0.162988364779726, -0.0342272499006952,
     55, 0.015952514913011, 0.965105063073668, 89.522, 0.254537224053518,
     0.0250741609732361, 1.56727315713416, 71.043393329918, 18.478606670082,
     1.5751327482868, 0.0877816809805793, -0.212788773489349, -0.131268075303869,
     0.213183823711587, 71, 0.0276409661071235, 0.992911428679848,
     63.73, -0.334757422606231, 0.0456655402031894, 3.6675938385894,
     81.3732740893709, -17.6432740893709, -1.52006657058606, 0, 0,
     0, 0, 0.90286003642222, -1.71808979012871, -0.629668182349073,
     1.32746128347647, 78, 0.739186815708686, 0.180517545659115,
     10, -2.15073367272139, 0.0752074627897638, 6.68086994241998,
     78.8949230433909, -68.8949230433909, -6.02974284991784, 0.171957849359207,
     -0.135174269380703, 0.15402827281771, -0.394817558679016, 83,
     0.0805418765069442, 0.999911882483083, 20.206, -0.574301659827085,
     0.0895740259404072, 8.14625938378561, 40.7204902450397, -20.5144902450397,
     -1.80955646882961, 0, 0, 0.0155013632655135, 0.0937792669936087,
     -0.301016429027622, 0.305854322538233, 87, 0.0831737676853215,
     0.712455955050213, 95.97, 0.604304349750242, 0.0330997284168613,
     2.38588103638393, 59.5484084127922, 36.4215915872078, 3.11746659898944
    ))

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_residualsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(103, 12.5778150132459, 91.6438423748575, 74.3436699029126, 21.4345870130922,
     "Predicted Value", 103, 11.7053332176182, 36.4215915872078,
     -2.06954194881583e-16, -68.8949230433909, "Residual", 103, 1,
     1.37545133663723, 4.80198405311172e-16, -4.20654007346277, "Std. Predicted Value",
     103, 1.01682494727761, 3.11746659898944, -0.00290022842524805,
     -6.02974284991784, "Std. Residual"))

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 0, 0, 17.1818583466082, 0, 0, 102, "M<unicode>", "", 0.732040432385989,
     0.535883194647866, 0.535883194647866, 11.8813631436676, 0.521819049031135,
     3, 99, "M<unicode>", 1.85904462451185e-16))

})

