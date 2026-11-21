context("Linear Regression - Term Specification")

# The term specification of the Model section (FactorsForm `advancedAvailable`): terms are defined once (a variable
# with a transformation or coding, a variable possibly several times, e.g. x and log(x)), and every model has its own
# intercept and terms built from these definitions, interactions included. jaspBase::prepareModelTerms() reads it; the
# fits must equal plain lm() fits of the same models.
#
# These tests call the analysis' own helpers: jaspTools::runAnalysis() formats `modelTerms` with the released
# FactorsForm description and drops the `definitions`, `terms` and `intercept` keys, until jaspTools and jaspSyntax
# know the term specification.

testData <- read.csv(system.file("extdata", "test.csv", package = "jaspTools"))

definition <- function(id, variable, specification, ...) list(id = id, variable = variable, specification = specification, ...)

definitions <- list(
  d1 = definition("d1", "contGamma", "none"),
  d2 = definition("d2", "contGamma", "log"),
  d3 = definition("d3", "contcor1",  "none"),
  d4 = definition("d4", "facGender", "treatment", referenceLevel = "m"),
  d5 = definition("d5", "facGender", "treatment", referenceLevel = "f"),
  d6 = definition("d6", "facGender", "sum"),
  d7 = definition("d7", "contGamma", "offset"),
  d8 = list(id = "d8", specification = "offsetConstant", value = 2)
)

# A model as the form gives it: the definitions its terms use, its terms (definition ids per term) and its variables
advancedModel <- function(number, terms, intercept = TRUE) {
  used       <- unique(unlist(terms))
  variables  <- lapply(definitions[used], `[[`, "variable") # a constant offset has none
  components <- Filter(length, unique(lapply(terms, function(operands) unname(unique(unlist(variables[operands]))))))
  list(name = paste0("model", number), title = paste("Model", number), components = components, intercept = intercept,
       definitions = unname(definitions[used]),
       terms = lapply(seq_along(terms), function(index) list(id = sprintf("t%d", index), operands = terms[[index]])))
}

advancedModels <- list(
  advancedModel(0, list("d1")),
  advancedModel(1, list("d1", "d2", "d3", "d4", c("d2", "d3")))
)

advancedOptions <- function(modelTerms, method = "enter") {
  list(dependent = "contNormal", covariates = c("contGamma", "contcor1"), factors = "facGender", weights = "",
       method = method, interceptTerm = TRUE, modelTerms = modelTerms)
}

fitAdvanced <- function(modelTerms) {
  options <- advancedOptions(modelTerms)
  data    <- testData
  data$facGender <- factor(data$facGender)
  jaspRegression:::.linregGetModelAdvanced("contNormal", options, data, weights = rep(1, nrow(data)))
}

test_that("Advanced models equal their lm() fits, with display labels", {
  model <- fitAdvanced(advancedModels)
  fit   <- model[[2]][["fit"]]

  data <- testData
  data$facGender <- relevel(factor(data$facGender), "m")
  reference <- lm(contNormal ~ contGamma + log(contGamma) + contcor1 + facGender + log(contGamma):contcor1, data = data)

  expect_equal(unname(attr(fit, "advancedLabels")),
               c("(Intercept)", "contGamma", "log(contGamma)", "contcor1", "facGender (f)",
                 paste0("log(contGamma)", jaspBase::interactionSymbol, "contcor1")))
  expect_equal(unname(coef(fit)), unname(coef(reference)), tolerance = 1e-8)
  expect_equal(unname(sqrt(diag(vcov(fit)))), unname(sqrt(diag(vcov(reference)))), tolerance = 1e-8)
  expect_equal(attr(fit, "advancedCategorical"), c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE))
  expect_true(model[[2]][["nested"]])
})

test_that("Models that are not nested, or differ in their intercept, are marked for the R-squared change", {
  # model 1 has log(contGamma) only inside an interaction, not as the main effect model 0 has
  notNested <- list(advancedModel(0, list("d2")), advancedModel(1, list("d1", "d3", "d4", c("d2", "d3"))))
  model <- fitAdvanced(notNested)
  expect_false(model[[2]][["nested"]])
  expect_true(model[[2]][["comparable"]])

  noIntercept <- list(advancedModel(0, list("d1"), intercept = FALSE), advancedModels[[2]])
  model <- fitAdvanced(noIntercept)
  expect_equal(attr(stats::terms(model[[1]][["fit"]]), "intercept"), 0L)
  expect_false(model[[2]][["comparable"]])
  expect_false(model[[2]][["nested"]])
})

test_that("The term specification refuses stepwise methods", {
  options <- advancedOptions(advancedModels, method = "backward")
  expect_error(jaspRegression:::.linregCheckErrors(testData, options), "Enter method only")
})

test_that("A model that cannot be fitted is skipped, and nesting is that of the fitted models", {
  skipped <- list(advancedModel(0, list("d1")), advancedModel(1, list(), intercept = FALSE), advancedModel(2, list("d3")))
  model <- fitAdvanced(skipped)
  expect_equal(vapply(model, `[[`, integer(1), "number"), c(1L, 3L))
  expect_false(model[[2]][["nested"]])

  nestedAcross <- list(advancedModel(0, list("d1")), advancedModel(1, list(), intercept = FALSE), advancedModel(2, list("d1", "d3")))
  expect_true(fitAdvanced(nestedAcross)[[2]][["nested"]])
})

test_that("Models with different offsets are not compared by their R-squared", {
  offsets <- list(advancedModel(0, list("d3", "d7")), advancedModel(1, list("d3", "d1")))
  model   <- fitAdvanced(offsets)
  expect_false(model[[2]][["comparable"]])
  expect_true(fitAdvanced(list(advancedModel(0, list("d3", "d7")), advancedModel(1, list("d3", "d4", "d7"))))[[2]][["comparable"]])
})

test_that("Stepping methods need a second model", {
  options <- advancedOptions(list(list(name = "model0", title = "Model 0", components = list("contGamma"))), method = "backward")
  expect_error(jaspRegression:::.linregCheckErrors(testData, options), "need a second model")
})

test_that("The Durbin-Watson test of a model with an offset is the test of the model itself", {
  fit <- fitAdvanced(list(advancedModel(0, list("d3", "d7"))))[[1]][["fit"]]
  dw  <- jaspRegression:::.linregGetDurBinWatsonTestResults(fit, "")
  expect_equal(dw[["dw"]], sum(diff(residuals(fit))^2) / sum(residuals(fit)^2), tolerance = 1e-8)

  # the same regression written without an offset: the response minus the offset on the same columns
  data      <- testData
  data$yAdj <- data$contNormal - data$contGamma
  reference <- lmtest::dwtest(lm(yAdj ~ contcor1, data = data), alternative = "two.sided")
  expect_equal(dw[["p"]], reference$p.value, tolerance = 1e-8)
  expect_equal(dw[["dw"]], unname(reference$statistic), tolerance = 1e-8)
})

test_that("The covariance matrix keys each model's coefficients by its own labels", {
  codings <- list(advancedModel(0, list("d5")), advancedModel(1, list("d6")))
  model   <- fitAdvanced(codings)
  columns <- unique(unlist(lapply(model, function(m) setdiff(attr(m[["fit"]], "advancedLabels"), "(Intercept)"))))
  for (singleModel in model) {
    covariance <- jaspRegression:::.linregGetCovarianceMatrixAdvanced(singleModel[["fit"]], columns)
    expect_equal(nrow(covariance), 1L)
    expect_equal(covariance[[covariance[["name"]]]], unname(vcov(singleModel[["fit"]])[2, 2]))
  }

  # the columns leave out the intercepts by R's name: a first model without intercept keeps its first predictor
  mixed <- fitAdvanced(list(advancedModel(0, list("d3"), intercept = FALSE), advancedModel(1, list("d1", "d3"))))
  expect_equal(jaspRegression:::.linregPredictorNames(mixed), c("contcor1", "contGamma"))
})

test_that("The variables of the term definitions are harmonized with the data like the components", {
  testthat::local_mocked_bindings(.harmonizeVariableEncoding = function(varNames, dataset) paste0("encoded_", varNames), .package = "jaspRegression")
  options <- jaspRegression:::.encodeModelTerms(advancedOptions(list(advancedModel(0, list("d1", "d4", "d8")))), testData)
  definitions <- options$modelTerms[[1]]$definitions
  expect_equal(vapply(definitions[1:2], `[[`, "", "variable"), c("encoded_contGamma", "encoded_facGender"))
  expect_null(definitions[[3]][["variable"]])   # a constant offset has none
})

test_that("An offset enters with its coefficient fixed at 1 and stays in the part and partial correlations", {
  model <- fitAdvanced(list(advancedModel(0, list("d3", "d4", "d7"))))
  fit   <- model[[1]][["fit"]]

  data <- testData
  data$facGender <- relevel(factor(data$facGender), "m")
  reference <- lm(contNormal ~ contcor1 + facGender + offset(contGamma), data = data)
  expect_equal(unname(coef(fit)), unname(coef(reference)), tolerance = 1e-8)

  # the offset is the last term of the model and no term of the fit: the display labels skip it
  expect_equal(model[[1]][["labels"]][3], "offset(contGamma)")
  expect_equal(unname(attr(fit, "advancedTermLabels")), model[[1]][["labels"]][1:2])

  # without contcor1 the refit keeps the offset
  cors    <- jaspRegression:::.linregGetPartAndPartialCorrelationAdvanced(fit)
  reduced <- lm(contNormal ~ facGender + offset(contGamma), data = data)
  partR2  <- summary(fit)[["r.squared"]] - summary(reduced)[["r.squared"]]
  expect_equal(unname(cors[["contcor1"]][["part"]]), sign(coef(fit)[[2]]) * sqrt(partR2), tolerance = 1e-8)
})

test_that("A constant offset goes into lm() as its offset argument", {
  model <- fitAdvanced(list(advancedModel(0, list("d3", "d8"), intercept = FALSE)))
  fit   <- model[[1]][["fit"]]

  reference <- lm(contNormal ~ 0 + contcor1, offset = rep(2, nrow(testData)), data = testData)
  expect_equal(unname(coef(fit)), unname(coef(reference)), tolerance = 1e-8)
  expect_equal(unname(fitted(fit)), unname(fitted(reference)), tolerance = 1e-8)
  expect_equal(model[[1]][["labels"]], c("contcor1", "offset(2)"))
  expect_equal(attr(fit, "advancedOffsets"), "offset(2)")
})

test_that("Further specifications fit as their lm() formulas; rank terms are marked as sample-dependent", {
  data <- testData
  model <- list(name = "model0", title = "Model 0", components = list("contGamma", "contcor1"), intercept = TRUE,
                definitions = list(definition("d1", "contGamma", "center"), definition("d2", "contcor1", "rank")),
                terms = list(list(id = "t1", operands = "d1"), list(id = "t2", operands = "d2")))
  fit <- fitAdvanced(list(model))[[1]][["fit"]]

  reference <- lm(contNormal ~ I(contGamma - mean(contGamma)) + rank(contcor1), data = data)
  expect_equal(unname(coef(fit)), unname(coef(reference)), tolerance = 1e-8)
  expect_true(attr(fit, "sampleDependent"))
  expect_false(attr(fitAdvanced(advancedModels)[[1]][["fit"]], "sampleDependent"))
})

test_that("Predictions on new data and bootstrap refits keep both kinds of offset", {
  model <- fitAdvanced(list(advancedModel(0, list("d3", "d7", "d8"))))
  fit   <- model[[1]][["fit"]]
  coefs <- unname(coef(fit))

  # one row of settings, as the response optimizer asks: offset(contGamma) from the row, offset(2) from the fit
  prediction <- jaspRegression:::.linregPredictFromSettings(fit, list(contcor1 = 0.5, contGamma = 1.5), testData, c("contcor1", "contGamma"), NULL)
  expect_equal(prediction, coefs[1] + coefs[2] * 0.5 + 1.5 + 2)

  # a refit on resampled rows with weights: the weights and the constant offset follow the rows
  data  <- testData
  data$w <- seq_len(nrow(data)) / nrow(data)
  rows  <- c(3:nrow(data), 1, 1)
  refit <- jaspRegression:::.linregRefitAdvanced(fit, data[rows, ], "w")
  reference <- lm(contNormal ~ contcor1 + offset(contGamma), data = data[rows, ], weights = w, offset = rep(2, nrow(data)))
  expect_equal(unname(coef(refit)), unname(coef(reference)), tolerance = 1e-8)
  expect_equal(unname(coef(jaspRegression:::.linregRefitAdvanced(fit, testData, ""))), coefs, tolerance = 1e-8)
})

test_that("A model without intercept and with only offsets has nothing to estimate and is skipped", {
  onlyOffsets <- list(advancedModel(0, list("d7", "d8"), intercept = FALSE), advancedModel(1, list("d3")))
  expect_equal(vapply(fitAdvanced(onlyOffsets), `[[`, integer(1), "number"), 2L)
  expect_error(fitAdvanced(onlyOffsets[1]), "No model can be fitted")

  # with its intercept it is a model: the offsets are fixed, the intercept is estimated
  withIntercept <- fitAdvanced(list(advancedModel(0, list("d8"))))[[1]][["fit"]]
  expect_equal(unname(coef(withIntercept)), mean(testData$contNormal) - 2, tolerance = 1e-8)
})
