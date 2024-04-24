context("Logistic Regression")

# Below are the unit tests for Andy Field's book

# Chapter 10
test_that("Fields Book - Chapter 10 results match", {
  options <- initClassicalRegressionOptions("RegressionLogistic")
  options$dependent <- "delivered"
  options$factors <- c("treat")
  options$modelTerms <- list(
      list(components= NULL, name="model0", title = "Model 0"),
      list(components=c("treat"), name="model1", title = "Model 1")
  )
  options$descriptives <- TRUE
  options$oddsRatio <- TRUE
  options$coefficientCi <- TRUE
  options$coefficientCiAsOddsRatio <- TRUE
  
  results <- jaspTools::runAnalysis("RegressionLogistic", dataset = "santas_log.csv", options)
  output1 <- results[["results"]][["factorDescriptives"]][["data"]]
  jaspTools::expect_equal_tables(output1,
                      list(0, 178, "TRUE",
                           1, 222, "FALSE")
  )
  output2 <- results[["results"]][["modelSummary"]][["data"]]
  jaspTools::expect_equal_tables(output2,
                                 list(531.250590526391, 535.242055073499, 529.250590526391, 399, 0,
                                      "M<unicode>", 0, 464.494488544052, 472.477417638268, 0.157928418719392,
                                      68.7561019823392, 460.494488544052, 398, 0.129912187559308,
                                      "M<unicode>", 0.215249724800201, 1.11022302462516e-16, 0.162128420464791
                                 )
  )
  output3 <- results[["results"]][["estimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(output3,
                                 list("TRUE", 1.36124731046877, 2.04061213301582, 0.510825623765981,
                                      "M<unicode>", 1.66666666666665, "(Intercept)", 7.57353027162946e-07,
                                      0.103279553032665, 1, 24.4633905355499, 4.94604797141616, "TRUE",
                                      3.57850992169288, 8.01981276525923, 1.67843078320261, "M<unicode>",
                                      5.35714285329404, "(Intercept)", 3.54594990009727e-16, 0.205863115653286,
                                      1, 66.4736903106761, 8.15313990501056, "FALSE", 0.0944511613464108,
                                      0.247860077995534, -1.87728164144707, "M<unicode>", 0.15300546466741,
                                      "treat (1)", 2.39469198254028e-14, 0.246122251011963, 1, 58.1777752035539,
                                      -7.62743568989958)
  )
  options <- initClassicalRegressionOptions("RegressionLogistic")
  options$dependent <- "delivered"
  options$factors <- c("treat")
  options$covariates <- c("quantity")
  options$modelTerms <- list(
    list(components = NULL, name="model0", title = "Model 0"),
    list(components=list("treat", "quantity", list("treat", "quantity")),
         name="model1", title = "Model 1")
  )
  options$residualCasewiseDiagnostic <- FALSE
  options$oddsRatio <- TRUE
  options$coefficientCi <- TRUE
  options$coefficientCiAsOddsRatio <- TRUE
  results <- jaspTools::runAnalysis("RegressionLogistic", dataset = "santas_log.csv", options)
  output4 <- results[["results"]][["modelSummary"]][["data"]]
  jaspTools::expect_equal_tables(output4,
                                 list(531.250590526391, 535.242055073499, 529.250590526391, 399, 0,
                                      "M<unicode>", 0, 398.18503428225, 414.150892470682, 0.293663757435006,
                                      139.065556244141, 390.18503428225, 396, 0.262759378512601, "M<unicode>",
                                      0.400251224473992, 0, 0.331341240867328)
  )
  output5 <- results[["results"]][["estimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(output5,
                                 list("TRUE", 1.36124731046877, 2.04061213301582, 0.510825623765981,
                                      "M<unicode>", 1.66666666666665, "(Intercept)", 7.57353027162946e-07,
                                      0.103279553032665, 1, 24.4633905355499, 4.94604797141616, "TRUE",
                                      2.95341293211689, 13.1506047478064, 1.8297145868812, "M<unicode>",
                                      6.23210767937535, "(Intercept)", 1.56809395778112e-06, 0.381003510779674,
                                      1, 23.0626254414104, 4.80235623849484, "FALSE", 0.440558078388745,
                                      3.38270938575308, 0.199481993012083, "M<unicode>", 1.22077022683835,
                                      "treat (1)", 0.701264521685676, 0.520006998514146, 1, 0.147159742726858,
                                      0.383614054391724, "FALSE", 0.663633201382873, 1.28147885872813,
                                      -0.0810054601474337, "M<unicode>", 0.922188656144837, "quantity",
                                      0.629417289982631, 0.167870548715548, 1, 0.23285178241894, -0.482547181546986,
                                      "FALSE", 0.22755778991235, 0.562741214139028, -1.02764323147157,
                                      "M<unicode>", 0.357849335589812, "treat (1) * quantity", 8.6229655831139e-06,
                                      0.230977621709245, 1, 19.7945175736481, -4.44910300775877)
  )

  options <- initClassicalRegressionOptions("RegressionLogistic")
  options$dependent <- "delivered"
  options$covariates <- c("quantity")
  options$modelTerms <- list(
    list(components = NULL, name="model0", title = "Model 0"),
    list(components= list("quantity"), name="model1", title = "Model 1")
  )
  options$oddsRatio <- TRUE
  options$coefficientCi <- TRUE
  options$coefficientCiAsOddsRatio <- TRUE
  options$conditionalEstimatePlot <- TRUE
  options$conditionalEstimatePlotPoints <- FALSE
  options$residualCasewiseDiagnostic <- FALSE
  results <- jaspTools::runAnalysis("RegressionLogistic", dataset = "santas_log_subset_treat0.csv", options)
  output6 <- results[["results"]][["estimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(output6,
                                 list("TRUE", 3.57850992169287, 8.01981276525911, 1.6784307832026, "M<unicode>",
                                      5.35714285329399, "(Intercept)", 3.54594990009555e-16, 0.205863115653283,
                                      1, 66.4736903106765, 8.15313990501062, "TRUE", 2.95341293211689,
                                      13.1506047478064, 1.8297145868812, "M<unicode>", 6.23210767937535,
                                      "(Intercept)", 1.56809395778108e-06, 0.381003510779673, 1, 23.0626254414104,
                                      4.80235623849485, "FALSE", 0.663633201382873, 1.28147885872813,
                                      -0.0810054601474335, "M<unicode>", 0.922188656144837, "quantity",
                                      0.629417289982632, 0.167870548715548, 1, 0.232851782418939,
                                      -0.482547181546985)
  )
  unnumberedFigureA <- results[["state"]][["estimatesPlots"]][["collection"]][[1]]
  #expect_equal_plots(unnumberedFigureA, "?", dir="Ancova") # This command needs to be updated

  options <- initClassicalRegressionOptions("RegressionLogistic")
  options$dependent <- "delivered"
  options$covariates <- c("quantity")
  options$modelTerms <- list(
    list(components = NULL, name="model0", title = "Model 0"),
    list(components= list("quantity"), name="model1", title = "Model 1")
  )
  options$oddsRatio <- TRUE
  options$coefficientCi <- TRUE
  options$coefficientCiAsOddsRatio <- TRUE
  options$conditionalEstimatePlot <- TRUE
  options$conditionalEstimatePlotPoints <- FALSE
  options$residualCasewiseDiagnostic <- FALSE
  results <- jaspTools::runAnalysis("RegressionLogistic", dataset = "santas_log_subset_treat1.csv", options)
  output7 <- results[["results"]][["estimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(output7,
                                 list("TRUE", 0.629242023574777, 1.06773288827061, -0.198850858244477,
                                      "M<unicode>", 0.819672131557941, "(Intercept)", 0.140449081313175,
                                      0.134894551619842, 1, 2.17303199956226, -1.47412075474238, "TRUE",
                                      3.80216170583388, 15.2232426986615, 2.02919657989328, "M<unicode>",
                                      7.60797150543205, "(Intercept)", 9.81760318548822e-09, 0.353897729969058,
                                      1, 32.8770384443136, 5.73385022862593, "FALSE", 0.241810977511836,
                                      0.450364312443746, -1.10864869161901, "M<unicode>", 0.33000459788989,
                                      "quantity", 2.78888928052724e-12, 0.158651002531974, 1, 48.8317463436682,
                                      -6.98797154714214)
  )
  unnumberedFigureB <- results[["state"]][["estimatesPlots"]][["collection"]][[1]]
  #expect_equal_plots(unnumberedFigureB, "?", dir="Ancova") # This command needs to be updated

  options <- initClassicalRegressionOptions("RegressionLogistic")
  options$dependent <- "delivered"
  options$factors <- c("treat")
  options$covariates <- c("quantity")
  options$modelTerms <- list(
    list(components = NULL, name="model0", title = "Model 0"),
    list(components=list("treat", "quantity", list("treat", "quantity")),
         name="model1", title = "Model 1")
  )
  options$residualCasewiseDiagnostic <- TRUE
  options$residualCasewiseDiagnosticZThreshold <- 2
  options$coefficientBootstrap <- TRUE
  options$coefficientBootstrapSamples <- 1000
  set.seed(1) # For Bootstrapping Unit Tests
  results <- jaspTools::runAnalysis("RegressionLogistic", dataset = "santas_log.csv", options)
  output9 <- results[["results"]][["influenceTable"]][["data"]]
  # used to output pearson resdual, switched to deviance using rstandard for std residuals
  jaspTools::expect_equal_tables(output9,
                                 list(51, 0.027905727978634, 0, 0.861727722493428, -7.23210767937535,
                                      -2.00666634304773, 93, 0.0272626386242436, 1, 0.0827619829298749,
                                      12.0828424428558, 2.24324229328406, 105, 0.027905727978634,
                                      0, 0.861727722493428, -7.23210767937535, -2.00666634304773,
                                      111, 0.027905727978634, 0, 0.861727722493428, -7.23210767937535,
                                      -2.00666634304773, 188, 0.0272626386242436, 1, 0.0827619829298749,
                                      12.0828424428558, 2.24324229328406, 214, 0.0251000549472288,
                                      0, 0.883828611727055, -8.60797150543206, -2.08841173681896,
                                      265, 0.0272626386242436, 1, 0.0827619829298749, 12.0828424428558,
                                      2.24324229328406, 307, 0.0251000549472288, 0, 0.883828611727055,
                                      -8.60797150543206, -2.08841173681896, 382, 0.0251000549472288,
                                      0, 0.883828611727055, -8.60797150543206, -2.08841173681896
                                 )
  )
  output10 <- results[["results"]][["estimatesTableBootstrapping"]][["data"]]
  jaspTools::expect_equal_tables(output10,
                      list("TRUE", 0.0499135607262049, 1.8487167467187, "(Intercept)", 0.408753000376651,
                           "FALSE", -0.00676602090084683, 0.221423402843459, "treat (1)",
                           0.545714072912926, "FALSE", -0.0107304860810226, -0.0951900669778652,
                           "quantity", 0.176386961737937, "FALSE", -0.0115090562401219,
                           -1.0338758817337, "treat (1) * quantity", 0.245734251200521
                      )
  )
})

# test the methods for entering predictors
options <- initClassicalRegressionOptions("RegressionLogistic")
options$residualCasewiseDiagnostic <- FALSE
options$covariates <- list("contNormal")
options$dependent <- "contBinom"
options$modelTerms <- options$modelTerms <- list(
  list(components= NULL, name="model0", title = "Model 0"),
  list(components=c("contNormal"), name="model1", title = "Model 1")
)
#backward
test_that("Method=backward model summary table results match", {
  options$method <- "backward"
  results <- jaspTools::runAnalysis("RegressionLogistic", "debug.csv", options)
	table <- results[["results"]][["modelSummary"]][["data"]]
	jaspTools::expect_equal_tables(table,
    list(139.466477068092, 144.676817440068, 0, 135.466477068092, 98, 0,
         1, 0, 0.00577009513822135, 138.058400038431, 140.663570224419,
         -0.00590174557777201, 0.59192297033843, 136.058400038431, 99,
         -0.00435050662194492, 2, -0.00793790496945946, 0.441676479938567,
         0))
})

#forward
test_that("Method=forward model summary table results match", {
  options$method <- "forward"
  results <- jaspTools::runAnalysis("RegressionLogistic", "debug.csv", options)
	table <- results[["results"]][["modelSummary"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1, 136.058400038431, 138.058400038431, 140.663570224419, 99, 0, 0))
})

#stepwise
test_that("Method=stepwise model summary table results match", {
  options$method <- "stepwise"
  results <- jaspTools::runAnalysis("RegressionLogistic", "debug.csv", options)
	table <- results[["results"]][["modelSummary"]][["data"]]
	jaspTools::expect_equal_tables(table,
    list(138.058400038431, 140.663570224419, 136.058400038431, 99, 0, 1,0))
})

test_that("Confusion Matrix Table Matches", {
    options <- initClassicalRegressionOptions("RegressionLogistic")
  options$residualCasewiseDiagnostic <- FALSE
  
  options$covariates <- c("contNormal", "contOutlier")
  options$dependent  <- "facGender"
  options$modelTerms <- options$modelTerms <- list(
    list(components= NULL, name="model0", title = "Model 0"),
    list(components=c("contNormal", "contOutlier"), name="model1", title = "Model 1")
  )
  options$residualCasewiseDiagnostic <- FALSE
  options$confusionMatrix <- TRUE
  results <- jaspTools::runAnalysis("RegressionLogistic", "debug.csv", options)
  table <- results[["results"]][["perfDiag"]][["collection"]][["perfDiag_confusionMatrix"]][["data"]]
  jaspTools::expect_equal_tables(table, list("f",                 34, 16, 68.00,
                                             "m",                 21, 29, 58.00,
                                             "Overall % Correct", "", "", 63.00))

})

test_that("Performance Metrics Table Matches", {
    options <- initClassicalRegressionOptions("RegressionLogistic")
  options$covariates <- list("contNormal")
  options$dependent  <- "contBinom"
  options$modelTerms <- options$modelTerms <- list(
    list(components= NULL, name="model0", title = "Model 0"),
    list(components=c("contNormal"), name="model1", title = "Model 1")
  )
  options$residualCasewiseDiagnostic <- FALSE
  options$accuracy  <- TRUE
  options$auc  <- TRUE
  options$sensitivity <- TRUE
  options$specificity <- TRUE
  options$precision <- TRUE
  options$fMeasure <- TRUE
  options$brierScore  <- TRUE
  options$hMeasure <- TRUE
  results <- jaspTools::runAnalysis("RegressionLogistic", "debug.csv", options)
  table <- results[["results"]][["perfDiag"]][["collection"]][["perfDiag_performanceMetrics"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list("Accuracy", 0.590, "AUC", 0.529556650246305, "Sensitivity", 0.0238095238095238, "Specificity",
                           1, "Precision", 1, "F-measure", 0.0465116279069767, "Brier score",
                           0.242217791647847, "H-measure", 0.0686265172331011)
  )
})

test_that("Confusion Matrix Table Matches", {
  options <- initClassicalRegressionOptions("RegressionLogistic")
  options$covariates <- c("contNormal", "contOutlier")
  options$factors <- c("facFive")
  options$dependent <- "facGender"
  options$residualCasewiseDiagnostic <- FALSE
  options$modelTerms <- options$modelTerms <- list(
    list(components= NULL, name="model0", title = "Model 0"),
    list(components=c("contNormal", "contOutlier", "facFive"), name="model1", title = "Model 1")
  )

  options$multicollinearity <- TRUE
  results <- jaspTools::runAnalysis("RegressionLogistic", "debug.csv", options)
  table <- results[["results"]][["multicolliTable"]][["data"]]
  jaspTools::expect_equal_tables(table, list("contNormal",   0.9536754, 1.048575,
                                             "contOutlier",  0.9742628, 1.026417,
                                             "facFive",      0.9377347, 1.0664))

  options <- initClassicalRegressionOptions("RegressionLogistic")
  options$covariates <- c("contNormal", "contOutlier")
  options$dependent <- "facGender"
  options$residualCasewiseDiagnostic <- FALSE
  options$modelTerms <- options$modelTerms <- list(
    list(components= NULL, name="model0", title = "Model 0"),
    list(components=c("contNormal", "contOutlier"), name="model1", title = "Model 1")
  )

  options$multicollinearity <- TRUE
  results <- jaspTools::runAnalysis("RegressionLogistic", "debug.csv", options)
  table <- results[["results"]][["multicolliTable"]][["data"]]
  jaspTools::expect_equal_tables(table, list("contNormal",   0.9958289, 1.004189,
                                             "contOutlier",  0.9958289, 1.004189))

})

test_that("Error Handling", {
  # factor levels not equal to 2
    options <- initClassicalRegressionOptions("RegressionLogistic")
  options$covariates <- list("contNormal")
  options$dependent <- "facFive"
  results <- jaspTools::runAnalysis("RegressionLogistic", "debug.csv", options)
  status <- results[["status"]]
  expect_identical(status, "validationError")

  # infinity check
    options <- initClassicalRegressionOptions("RegressionLogistic")
  options$covariates <- list("debInf")
  options$dependent <- "contBinom"
  results <- jaspTools::runAnalysis("RegressionLogistic", "debug.csv", options)
  status <- results[["status"]]
  expect_identical(status, "validationError")

  # <2 observations
    options <- initClassicalRegressionOptions("RegressionLogistic")
  options$covariates <- list("debMiss99")
  options$dependent <- "contBinom"
  results <- jaspTools::runAnalysis("RegressionLogistic", "debug.csv", options)
  status <- results[["status"]]
  expect_identical(status, "validationError")

  # variance check
    options <- initClassicalRegressionOptions("RegressionLogistic")
  options$covariates <- list("debSame")
  options$dependent <- "contBinom"
  results <- jaspTools::runAnalysis("RegressionLogistic", "debug.csv", options)
  status <- results[["status"]]
  expect_identical(status, "validationError")
})

test_that("Pseudo R-squared are correct", {
  # Specifically, tests McFadden, Nagelkerke, Tjur, and Cox & Snell coefficients
  # Example from Tjur (2008) http://dx.doi.org/10.1198/tast.2009.08210, page 370
  # Tjur's results for Tjur R2: 0.096231
  #.
  # Check against code from performance package
  # library(aplore3)     # version 0.9
  # library(performance) # version 0.4.6
  # data("lowbwt", package = "aplore3")
  # fit <- glm(low ~ age + lwt + race + smoke, data = lowbwt, family = binomial)
  #
  # performance::r2_mcfadden(fit)$R2 # McFadden's R2: 0.08562914
  #
  # performance::r2_nagelkerke(fit) # Nagelkerke's R2: 0.1418442
  #
  # performance::r2_tjur(fit) # Tjur's R2: 0.09623107
  #
  # performance::r2_coxsnell(fit) # Cox & Snell's R2: 0.1008645

  options <- initClassicalRegressionOptions("RegressionLogistic")
  options$dependent  <- "low"
  options$covariates <- c("age", "lwt")
  options$factors    <- c("race", "smoke")
  options$modelTerms <- options$modelTerms <- list(
    list(components= NULL, name="model0", title = "Model 0"),
    list(components=c("age", "lwt", "race", "smoke"), name="model1", title = "Model 1")
  )
  options$residualCasewiseDiagnostic <- FALSE
  results <- jaspTools::runAnalysis("RegressionLogistic", "lowbwt.csv", options)
  r_squared <- results$results$modelSummary$data[[2]][c("fad", "nag", "tju", "cas")]
  jaspTools::expect_equal_tables(r_squared,
                      list(0.0856291418878957, 0.141844242772774, 0.0962310669224921, 0.100864461712579)
                      )


  # Another test, inspired by the issue: https://github.com/jasp-stats/jasp-issues/issues/2368
  # That issue was caused by a numerical overflow for a somewhat large dataset
  set.seed(1)
  n <- 1e5
  x <- runif(n, min = 0, max = 1)
  y_hat <- 5 * x
  p <- plogis(y_hat)
  y <- rbinom(n, 1, p)

  df <- data.frame(x, y)

  # library(performance) # version 0.10.5
  # fit <- glm(y~x, data = df, family = binomial)
  # performance::r2_mcfadden(fit)$R2 # 0.2066583
  # performance::r2_nagelkerke(fit) # 0.2767422
  # performance::r2_tjur(fit) # 0.1713724
  # performance::r2_coxsnell(fit) # 0.1524201

  options <- initClassicalRegressionOptions("RegressionLogistic")
  options$dependent <- "y"
  options$covariates <- "x"
  options$modelTerms <-   options$modelTerms <- options$modelTerms <- list(
    list(components= NULL, name="model0", title = "Model 0"),
    list(components=c("x"), name="model1", title = "Model 1")
  )
  options$residualCasewiseDiagnostic <- FALSE
  results <- jaspTools::runAnalysis("RegressionLogistic", df, options)
  r_squared <- results$results$modelSummary$data[[2]][c("fad", "nag", "tju", "cas")]
  jaspTools::expect_equal_tables(r_squared,
                                 list(0.206658284548968, 0.276742170706529, 0.171372447470285, 0.152420076012601)
  )
})

test_that("Performance plots match", {
  options <- initClassicalRegressionOptions("RegressionLogistic")
  options$dependent  <- "low"
  options$covariates <- c("age", "lwt")
  options$factors    <- c("race", "smoke")
  options$modelTerms <- options$modelTerms <- list(
    list(components= NULL, name="model0", title = "Model 0"),
    list(components=c("age", "lwt", "race", "smoke"), name="model1", title = "Model 1")
  )
  options$residualCasewiseDiagnostic <- FALSE
  
  options$rocPlot <- TRUE
  options$precisionRecallPlot  <- TRUE

  results <- jaspTools::runAnalysis("RegressionLogistic", "lowbwt.csv", options)

  plotName <- results[["results"]][["performancePlots"]][["collection"]][["performancePlots_rocPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "rocPlot")

  plotName <- results[["results"]][["performancePlots"]][["collection"]][["performancePlots_prPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "prPlot")

})
