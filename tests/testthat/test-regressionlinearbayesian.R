context("Bayesian Linear Regression")

# does not test
# - plots (will/ should be updated in the future)

test_that("Main tables results match", {
    set.seed(1)
    options <- jaspTools::analysisOptions("RegressionLinearBayesian")
    options$modelPrior <- "betaBinomial"
    options$priorRegressionCoefficients <- "gPrior"
    options$gPriorType <- "userDefined"
    options$gPriorG <- 1
    options$dependent <- "contNormal"
    options$covariates <- "contGamma"
    options$weights <- "facFifty"
    options$modelTerms <- list(
        list(components="contGamma", isNuisance=FALSE)
    )
    options$posteriorSummaryPlot <- TRUE
    options$posteriorSummaryTable <- TRUE
    options$descriptives <- TRUE
    options$setSeed <- TRUE
    options$residualsSavedToData   <-  FALSE
    options$residualSdsSavedToData <-  FALSE


    results <- jaspTools::runAnalysis("RegressionLinearBayesian", "test.csv", options)
    table <- results[["results"]][["basreg"]][["collection"]][["basreg_modelComparisonTable"]][["data"]]
    jaspTools::expect_equal_tables(
        table,
        list("Null model", 1, 1.41415898152264, 0.585777072821738, 0, 0.5,
             "contGamma", 0.707134072665074, 0.707134072665075, 0.414222927178262,
             1.55940279678024e-06, 0.5),
        label = "regressionTable"
    )

    table <- results[["results"]][["basreg"]][["collection"]][["basreg_postSumContainer"]][["collection"]][["basreg_postSumContainer_postSumTable"]][["data"]]
    jaspTools::expect_equal_tables(
        table,
        list(1, "Intercept", -0.477863289393241, -0.255843391953333, 0, 0,
             1, 1, 0.0990958319270768, -0.088008039721404, 0.707134072665075,
             "contGamma", -0.062603708459968, -0.000157637515978536, 0.585777072821738,
             0.5, 0.414222927178262, 0.5, 0.028020423972741, 0.0693220652388757
        ),
        label = "posteriorSummaryTable"
    )

    table <- results[["results"]][["descriptivesTable"]][["data"]]
    jaspTools::expect_equal_tables(
        table,
        list("contNormal", 100, -0.18874858754, 1.05841360919316, "contGamma",
             100, 2.03296079621, 1.53241112621044),
        label = "descriptivesTable"
    )

    plot <- results[["state"]][["figures"]][[1]][["obj"]]
    jaspTools::expect_equal_plots(plot, "posteriorCoefficientsWithCRI", "RegressionLinearBayesian")

    ybreaks <- jaspGraphs::getAxisBreaks(plot)[["y"]]
    testthat::expect_true(
      # to test if the ybreaks contain scientific notation, we print the ybreaks and check if there is any ... e-... in there
      !any(grepl("e-", capture.output(print(ybreaks)), fixed = TRUE)),
      label = "no scientific notation on the y-axis of the posterior coefficients plot"
    )

})

options <- jaspTools::analysisOptions("RegressionLinearBayesian")
options$covariates <- c("adverts", "airplay", "attract")
options$dependent <- "sales"
options$modelPrior <- "betaBinomial"
options$priorRegressionCoefficients <- "gPrior"
options$gPriorType <- "userDefined"
options$gPriorG <- 1
options$modelTerms <- list(list(components = "adverts", isNuisance = FALSE),
                           list(components = "airplay", isNuisance = FALSE),
                           list(components = c("adverts", "airplay"), isNuisance = FALSE))
options$posteriorSummaryTable <- TRUE
options$setSeed <- TRUE
options$residualsSavedToData   <-  FALSE
options$residualSdsSavedToData <-  FALSE
set.seed(1)
results <- jaspTools::runAnalysis("RegressionLinearBayesian", testthat::test_path("Album Sales.csv"), options)

test_that("Model Comparison - sales table results match", {
  table <- results[["results"]][["basreg"]][["collection"]][["basreg_modelComparisonTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(1, 5.270228526968, "adverts + airplay + adverts<unicode><unicode><unicode>airplay",
                           0.632270344815323, 0.7249054837023, 0.333333333333333,
                           1.13847052311215, 3.03592136194222, "adverts + airplay",
                           0.629285746598296, 0.275094508412474, 0.111111111111111, 8.35835904551863e-23,
                           4.29802141261388e-22, "airplay", 0.358703726117256, 5.37252676576735e-23,
                           0.111111111111111, 2.33573121426008e-24, 1.20107579948757e-23,
                           "adverts", 0.334648067623073, 1.50134474935946e-24, 0.111111111111111,
                           1.76804178544216e-40, 6.81869617963531e-40, "Null model", 0,
                           3.40934808981766e-40, 0.333333333333333),
                      label = "regressionTable")
})

test_that("Posterior Summaries of Coefficients table (all models) results match", {
  table <- results[["results"]][["basreg"]][["collection"]][["basreg_postSumContainer"]][["collection"]][["basreg_postSumContainer_postSumTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, "Intercept", 185.584190973766, 193.2, 0, 0, 1, 1, 3.4881473625465,
                                      199.210562797446, 125307915.6303, "adverts", 0.0270883689845171,
                                      0.0533840599976063, 0, 0.444444444444444, 1, 0.555555555555556,
                                      0.0153320338949993, 0.085090567547742, 532995013.836511,
                                      "airplay", 1.3615828086271, 1.95222334627145, 0, 0.444444444444444,
                                      1, 0.555555555555556, 0.306097117095416, 2.54152744076409, 5.270228526968,
                                      "adverts<unicode><unicode><unicode><unicode><unicode><unicode><unicode><unicode><unicode>airplay",
                                      -0.00139087184559238, -0.000322869356058448, 0.2750945162977,
                                      0.666666666666667, 0.7249054837023, 0.333333333333333, 0.000481709014770306,
                                      0.000426382703700553))
})

options$effectsType <- "matchedModels"
set.seed(1)
results <- jaspTools::runAnalysis("RegressionLinearBayesian", testthat::test_path("Album Sales.csv"), options)

test_that("Posterior Summaries of Coefficients table (matched models) results match", {
  table <- results[["results"]][["basreg"]][["collection"]][["basreg_postSumContainer"]][["collection"]][["basreg_postSumContainer_postSumTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, "Intercept", 185.584190973766, 193.2, 0, 0, 1, 1, 3.4881473625465,
                                      199.210562797446, 86178799.6466646, "adverts", 0.0270883689845171,
                                      0.0533840599976063, 5.37252676576734e-23, 0.444444444444444,
                                      0.275094509913426, 0.222222222222222, 0.0153320338949993, 0.085090567547742,
                                      366560012.351291, "airplay", 1.3615828086271, 1.95222334627145,
                                      1.50134474935946e-24, 0.444444444444444, 0.275094514796748,
                                      0.222222222222222, 0.306097117095416, 2.54152744076409, 0.878371446338705,
                                      "adverts<unicode><unicode><unicode><unicode><unicode><unicode><unicode><unicode><unicode>airplay",
                                      -0.00139087184559238, -0.000322869356058448, 0.275094508412474,
                                      0.111111111111111, 0.7249054837023, 0.333333333333333, 0.000481709014770306,
                                      0.000426382703700553))
})

test_that("Coefficient plots match", {
    set.seed(1)
    options <- jaspTools::analysisOptions("RegressionLinearBayesian")
    options$modelPrior <- "betaBinomial"
    options$priorRegressionCoefficients <- "gPrior"
    options$gPriorType <- "userDefined"
    options$gPriorG <- 1
    options$dependent <- "contNormal"
    options$covariates <- list("contGamma", "debCollin1", "contcor2")
    options$modelTerms <- list(
        list(components="contGamma", isNuisance=FALSE),
        list(components="debCollin1", isNuisance=FALSE),
        list(components="contcor2", isNuisance=FALSE)
    )
    options$inclusionProbabilitiesPlot <- TRUE
    options$marginalPosteriorPlot <- TRUE
    options$posteriorSummaryPlotWithoutIntercept <- FALSE
    options$residualsSavedToData <- FALSE
    options$residualSdsSavedToData <- FALSE

    results <- jaspTools::runAnalysis("RegressionLinearBayesian", "test.csv", options)

    inclusionProbabilities <- results[['state']][['figures']][[1]][["obj"]]
    jaspTools::expect_equal_plots(inclusionProbabilities, "inclusionProbabilities", "RegressionLinearBayesian")

    posteriorCoefficients <- results[['state']][['figures']][[2]][["obj"]]
    jaspTools::expect_equal_plots(posteriorCoefficients, "posteriorCoefficients", "RegressionLinearBayesian")
})

test_that("Residuals plots match", {
    set.seed(1)
    options <- jaspTools::analysisOptions("RegressionLinearBayesian")
    options$modelPrior <- "betaBinomial"
    options$priorRegressionCoefficients <- "gPrior"
    options$gPriorType <- "userDefined"
    options$gPriorG <- 1
    options$dependent <- "contNormal"
    options$covariates <- list("contGamma")
    options$modelTerms <- list(
        list(components="contGamma", isNuisance=FALSE)
    )
    options$residualsVsFittedPlot <- TRUE
    options$qqPlot <- TRUE

    results <- jaspTools::runAnalysis("RegressionLinearBayesian", "test.csv", options)

    residualsVsFitted <- results[['state']][['figures']][[1]][["obj"]]
    jaspTools::expect_equal_plots(residualsVsFitted, "residualsVsFitted", "RegressionLinearBayesian")

    qqPlot <- results[['state']][['figures']][[2]][["obj"]]
    jaspTools::expect_equal_plots(qqPlot, "qqPlot", "RegressionLinearBayesian")
})

test_that("Models plots match", {
    set.seed(1)
    options <- jaspTools::analysisOptions("RegressionLinearBayesian")
    options$modelPrior <- "betaBinomial"
    options$priorRegressionCoefficients <- "gPrior"
    options$gPriorType <- "userDefined"
    options$gPriorG <- 1
    options$dependent <- "contNormal"
    options$covariates <- list("contGamma", "contExpon", "contcor1")
    options$modelTerms <- list(
        list(components="contGamma", isNuisance=FALSE),
        list(components="contExpon", isNuisance=FALSE),
        list(components="contcor1", isNuisance=FALSE)
    )
    options$logPosteriorOddsPlot <- TRUE
    options$modelComplexityPlot <- TRUE
    options$modelProbabilitiesPlot <- TRUE

    results <- jaspTools::runAnalysis("RegressionLinearBayesian", "test.csv", options)

    logPosteriorOdds <- results[['state']][['figures']][[1]][["obj"]]
    jaspTools::expect_equal_plots(logPosteriorOdds, "logPosteriorOdds", "RegressionLinearBayesian")

    modelProbabilities <- results[['state']][['figures']][[2]][["obj"]]
    jaspTools::expect_equal_plots(modelProbabilities, "modelProbabilities", "RegressionLinearBayesian")

    modelComplexity <- results[['state']][['figures']][[3]][["obj"]]
    jaspTools::expect_equal_plots(modelComplexity, "modelComplexity", "RegressionLinearBayesian")
})

test_that("Model priors match", {
    options <- jaspTools::analysisOptions("RegressionLinearBayesian")
    options$dependent <- "contNormal"
    options$covariates <- list("contGamma", "contExpon", "contcor1")
    options$modelTerms <- list(
        list(components="contGamma", isNuisance=FALSE),
        list(components="contExpon", isNuisance=FALSE),
        list(components="contcor1", isNuisance=FALSE)
    )

    options$priorRegressionCoefficients <- "gPrior"
    options$gPriorType <- "userDefined"
    options$gPriorG <- 1

    modelPriors <- list(
        uniform      = list(modelPrior = "uniform"),
        betabinomial = list(modelPrior = "betaBinomial", betaBinomialParamA = 2, betaBinomialParamB = 3),
        wilson       = list(modelPrior = "wilson", wilsonParamLambda = 2),
        bernoulli    = list(modelPrior = "bernoulli", bernoulliParam = 0.75),
        castillo     = list(modelPrior = "castillo", castilloParamU = 2)
    )

    tables <- list(
      uniform = list(1, 1.57395624386139, "contcor1", 0.0259312818065143, 0.18357409334673, 0.125,
                     0.794010210849689, 1.19441559641437, "contExpon + contcor1", 0.0305485694813511,
                     0.145759704564778, 0.125, 0.741246077229015, 1.10254184869332,
                     "Null model", 0, 0.136073576574137, 0.125,
                     0.728427596222453, 1.08053229837658, "contGamma + contcor1", 0.0271156286160024,
                     0.133720435545275, 0.125, 0.582716803831538, 0.838497469885551,
                     "contGamma + contExpon + contcor1", 0.0320272344954692, 0.106971708941279,
                     0.125, 0.577952636377137, 0.830828431679437, "contExpon",
                     0.00394490646287671, 0.106097131220286, 0.125, 0.571678132607948,
                     0.820751022205754, "contGamma", 0.0035046856966956,
                     0.104945294879656, 0.125, 0.451360284108053, 0.632406344090376,
                     "contGamma + contExpon", 0.00794701370052631, 0.0828580549278585, 0.125),
      betabinomial = list(1, 1.09972042307322, "Null model", 0, 0.305501620632623, 0.285714285714286,
                          1.34907965211537, 1.52986352799304, "contcor1", 0.0259312818065142,
                          0.164858408033496, 0.114285714285714, 1.07118301902915, 1.16119361243271,
                          "contExpon + contcor1", 0.0305485694813511, 0.0981744444922654, 0.0857142857142857,
                          0.78613138299483, 0.823632568182213, "contGamma + contExpon + contcor1",
                          0.0320272344954692, 0.0960657646140345, 0.114285714285714,
                          0.779704141622828, 0.816189551972087, "contExpon", 0.00394490646287671,
                          0.095280351551897, 0.114285714285714, 0.771239336260693, 0.806406645977901,
                          "contGamma", 0.0035046856966956, 0.0942459468493083, 0.114285714285714,
                          0.98270684810302, 1.05578959235875, "contGamma + contcor1", 0.0271156286160024,
                          0.090065560410675, 0.0857142857142857, 0.608920975063185, 0.630469483121386,
                          "contGamma + contExpon", 0.00794701370052642, 0.0558079034157001, 0.0857142857142857),
      wilson    = list(1, 1.06012759862291, "Null model", 0, 0.679513393365172, 0.666666666666667,
                       1.34907965211537, 1.42361890683027, "contcor1", 0.0259312818065142,
                       0.114589711541102, 0.0833333333333333, 0.779704141622828, 0.780170359412781,
                       "contExpon", 0.00394490646287682, 0.0662274258868758, 0.0833333333333333,
                       0.771239336260693, 0.771106749727115, "contGamma", 0.00350468569669571,
                       0.0655084323099008, 0.0833333333333333, 1.07118301902915, 1.09427558421677,
                       "contExpon + contcor1", 0.0305485694813512, 0.0259958288634159,
                       0.0238095238095238, 0.98270684810302, 1.00168385524689, "contGamma + contcor1",
                       0.0271156286160025, 0.023848659465631278, 0.0238095238095238,
                       0.608920975063185, 0.614965084193061, "contGamma + contExpon",
                       0.00794701370052642, 0.0147774985020148, 0.0238095238095238,
                       0.78613138299483, 0.799366351112873, "contGamma + contExpon + contcor1",
                       0.0320272344954695, 0.00953905006588702, 0.0119047619047619),
      bernoulli = list(1, 0.86399952797011, "contGamma + contExpon + contcor1", 0.0320272344954695,
                       0.386685986331906, 0.421875, 1.36260050444544, 1.30198272344267,
                       "contExpon + contcor1", 0.0305485694813512, 0.175632840012613,
                       0.140625, 1.25005421404158, 1.17378768881987, "contGamma + contcor1",
                       0.0271156286160024, 0.161126148908341, 0.140625,
                       0.774579145719195, 0.677802664220887, "contGamma + contExpon",
                       0.00794701370052631, 0.0998396336515175, 0.140625,
                       1.71609947306256, 1.61856628588766, "contcor1", 0.0259312818065143,
                       0.0737324019316511, 0.046875, 0.991824214741921, 0.905048940898302,
                       "contExpon", 0.00394490646287671, 0.0426138360828165, 0.046875,
                       0.981056542129886, 0.894790951609985, "contGamma", 0.0035046856966956,
                       0.0421512018489849, 0.046875, 1.2720520025424, 1.16902822685253,
                       "Null model", 0, 0.0182179512321698, 0.015625),
      castillo = list(1, 1.05212996245477, "Null model", 0, 0.759406593919387, 0.75,
                      1.34907965211537, 1.40358876767347, "contcor1", 0.0259312818065142,
                      0.0931363621398984, 0.0681818181818182, 0.779704141622828, 0.777506836246215,
                      "contExpon", 0.00394490646287682, 0.0538284060413301, 0.0681818181818182,
                      0.771239336260693, 0.768591180409711, "contGamma", 0.00350468569669571,
                      0.0532440215860347, 0.0681818181818182, 1.07118301902915, 1.08588823864352,
                      "contExpon + contcor1", 0.0305485694813512, 0.0147902445080947,
                      0.0136363636363636, 0.98270684810302, 0.994963745431394, "contGamma + contcor1",
                      0.0271156286160025, 0.013568619278893999, 0.0136363636363636,
                      0.608920975063185, 0.613306973652703, "contGamma + contExpon",
                      0.00794701370052642, 0.008407610975251, 0.0136363636363636,
                      0.78613138299483, 0.795250327948295, "contGamma + contExpon + contcor1",
                      0.0320272344954695, 0.00361814155111055, 0.00454545454545455)
    )

    for (nm in names(modelPriors)) {
        set.seed(1)
        options[names(modelPriors[[nm]])] <- modelPriors[[nm]]
        results <- jaspTools::runAnalysis("RegressionLinearBayesian", "test.csv", options)
        table <- results[["results"]][["basreg"]][["collection"]][["basreg_modelComparisonTable"]][["data"]]
        jaspTools::expect_equal_tables(
            table,
            tables[[nm]],
            label = paste("regressionTable modelprior:", paste(modelPriors[[nm]], collapse = ""))
        )
    }
})

test_that("Exporting residuals works", {

  data("Hald", package = "BAS")
  Hald$W <- seq_len(nrow(Hald))
  options <- jaspTools::analysisOptions("RegressionLinearBayesian")
  options$dependent <- "Y"
  options$covariates <- paste0("X", 1:4)
  options$modelTerms <- lapply(options$covariates, function(x) list(components = x, isNuisance = FALSE))
  options$modelTerms[[1]]$isNuisance <- TRUE
  options$weights <- "W"
  options$modelPrior <- "betaBinomial"
  options$priorRegressionCoefficients <- "gPrior"
  options$gPriorType <- "userDefined"
  options$gPriorG <- 13
  options$residualsSavedToData   <- TRUE
  options$residualSdsSavedToData <- TRUE

  summaryTypes <- c("best", "complex", "median", "averaged")

  for (summaryType in summaryTypes) {
    options$summaryType <- summaryType

    options$residualsSavedToDataColumn   <- paste("residualsSavedToData",   "-", summaryType)
    options$residualSdsSavedToDataColumn <- paste("residualSdsSavedToData", "-", summaryType)

    results <- jaspTools::runAnalysis("RegressionLinearBayesian", Hald, options)

    # ideally we'd check the actual data here but the jaspTools output unfortunately does not contain the data
    testthat::expect_identical(
      results[["results"]][["basreg"]][["collection"]][["basreg_residualsSavedToDataColumn"]][c("columnName", "columnType")],
      list(columnName = options$residualsSavedToDataColumn, columnType = "scale")
    )

    testthat::expect_identical(
      results[["results"]][["basreg"]][["collection"]][["basreg_residualSdsSavedToDataColumn"]][c("columnName", "columnType")],
      list(columnName = options$residualSdsSavedToDataColumn, columnType = "scale")
    )

}

})

test_that("Refitted median model preserves selection and produces weighted predictions", {
  data("Hald", package = "BAS")
  weights <- seq_len(nrow(Hald))
  basModel <- BAS::bas.lm(
    Y ~ ., data = Hald, weights = weights, prior = "g-prior", alpha = 3, # intentionally not the more natural nrow(Hald), which is BAS's default, to test that the alpha parameter is preserved
    initprobs = c(1, 1, 0.5, 0.5, 0.5)
  )
  options <- list(
    dependent = "Y",
    modelTerms = lapply(paste0("X", 1:4), function(x) list(component = x))
  )

  medianModel <- jaspRegression:::.basregRefitMedianModel(basModel, Hald, options, weights)
  refittedPredictions <- predict(medianModel, estimator = "HPM", se.fit = TRUE)
  expectedModel <- (0:(basModel$n.vars - 1))[basModel$probne0 > 0.5]

  expect_equal(medianModel$which[[1]], expectedModel)
  expect_equal(medianModel$alpha, basModel$alpha)
  expect_equal(medianModel$prior, basModel$prior)
  expect_length(refittedPredictions$fit, nrow(Hald))
  expect_length(refittedPredictions$se.pred, nrow(Hald))
  expect_true(all(is.finite(refittedPredictions$fit)))
  expect_true(all(is.finite(refittedPredictions$se.pred)))
  expect_true(1 %in% medianModel$which[[1]])
})

test_that("Regression coefficient priors use their own parameter", {
    options <- list(
        gPriorG = 13,
        hyperGAlpha = 2.5,
        hyperGLaplaceAlpha = 3,
        hyperGNAlpha = 3.5,
        jzsRScale = 0.5,
        gPriorType = "userDefined"
    )

    expect_equal(jaspRegression:::.basregGetPriorParameter("g-prior", options, n = 100), 13)
    expect_equal(jaspRegression:::.basregGetPriorParameter("g-prior", modifyList(options, list(gPriorType = "n")), n = 100), 100)
    expect_equal(jaspRegression:::.basregGetPriorParameter("hyper-g", options, n = 100), 2.5)
    expect_equal(jaspRegression:::.basregGetPriorParameter("hyper-g-laplace", options, n = 100), 3)
    expect_equal(jaspRegression:::.basregGetPriorParameter("hyper-g-n", options, n = 100), 3.5)
    expect_equal(jaspRegression:::.basregGetPriorParameter("JZS", options, n = 100), 0.25)

    options$gPriorG <- NULL
    expect_equal(jaspRegression:::.basregGetPriorParameter("g-prior", options, n = 100), 100)

    options$hyperGAlpha <- 3.75
    expect_equal(jaspRegression:::.basregGetPriorParameter("g-prior", options, n = 100), 100)

    legacyOptions <- list(gPriorAlpha = 13, jzsRScale = 0.5, gPriorType = "userDefined")
    expect_equal(jaspRegression:::.basregGetPriorParameter("g-prior", legacyOptions, n = 100), 13)
    legacyOptions$gPriorType <- NULL
    expect_equal(jaspRegression:::.basregGetPriorParameter("g-prior", legacyOptions, n = 100), 13)
    expect_equal(jaspRegression:::.basregGetPriorParameter("hyper-g", legacyOptions, n = 100), 13)
    expect_equal(jaspRegression:::.basregGetPriorParameter("hyper-g-laplace", legacyOptions, n = 100), 13)
    expect_equal(jaspRegression:::.basregGetPriorParameter("hyper-g-n", legacyOptions, n = 100), 13)

    legacyOptions$gPriorG <- 25
    legacyOptions$hyperGAlpha <- 2.5
    expect_equal(jaspRegression:::.basregGetPriorParameter("g-prior", legacyOptions, n = 100), 25)
    expect_equal(jaspRegression:::.basregGetPriorParameter("hyper-g", legacyOptions, n = 100), 2.5)

    defaultOptions <- list(jzsRScale = 0.5, gPriorType = "userDefined")
    expect_equal(jaspRegression:::.basregGetPriorParameter("g-prior", defaultOptions, n = 100), 100)
    expect_equal(jaspRegression:::.basregGetPriorParameter("hyper-g", defaultOptions, n = 100), 3)
    expect_equal(jaspRegression:::.basregGetPriorParameter("hyper-g-laplace", defaultOptions, n = 100), 3)
    expect_equal(jaspRegression:::.basregGetPriorParameter("hyper-g-n", defaultOptions, n = 100), 3)
})

test_that("Model comparison Bayes factor titles identify their reference model", {
  expect_equal(jaspRegression:::.getModelComparisonBfTitle("BF10", "nullModelTop"), "\\(\\mathrm{BF}_{10}\\)")
  expect_equal(jaspRegression:::.getModelComparisonBfTitle("BF01", "nullModelTop"), "\\(\\mathrm{BF}_{01}\\)")
  expect_equal(jaspRegression:::.getModelComparisonBfTitle("LogBF10", "nullModelTop"), "\\(\\log(\\mathrm{BF}_{10})\\)")
  expect_equal(jaspRegression:::.getModelComparisonBfTitle("BF10", "bestModelTop"), "\\(\\mathrm{BF}_{1\\mathrm{B}}\\)")
  expect_equal(jaspRegression:::.getModelComparisonBfTitle("BF01", "bestModelTop"), "\\(\\mathrm{BF}_{\\mathrm{B}1}\\)")
  expect_equal(jaspRegression:::.getModelComparisonBfTitle("LogBF10", "bestModelTop"), "\\(\\log(\\mathrm{BF}_{1\\mathrm{B}})\\)")
})
