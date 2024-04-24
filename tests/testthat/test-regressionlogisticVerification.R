context("Logistic Regression -- Verification project")

# Below are the unit tests for Andy Field's book

## Testing Titanice

options <- initClassicalRegressionOptions("RegressionLogistic")
options$dependent <- "Survived"
options$factors <- c("PClass", "Sex")
options$covariates <- "Age"

options$modelTerms <- list(
  list(components= NULL, name="model0", title = "Model 0"),
  list(components=c("Age", "PClass", "Sex"), name="model1", title = "Model 1")
)

options$method <- "enter"
options$coefficientEstimate <- TRUE
results <- jaspTools::runAnalysis("RegressionLogistic", dataset = "LogReg.csv", options)

# https://jasp-stats.github.io/jasp-verification-project/regression.html#logistic-regression
test_that("Main table results match R, SPSS, SAS and MiniTab", {
  # Main table
  resultTable <- results$results$modelSummary$data
  
  jaspTools::expect_equal_tables(
    "test"=resultTable,
    "ref"=list(1027.57254724533, 1032.20058862151, "", "", 1025.57254724533,
               755, "", "H<unicode>", "", "", "", 705.140778094395, 728.280984975293,
               0.354079636893354, 330.431769150933, 695.140778094395, 751,
               0.322192486566131, "H<unicode>", 0.476901084612013, 0, 0.393957934297113)
  )
})

# https://jasp-stats.github.io/jasp-verification-project/regression.html#logistic-regression
test_that("Estimates table results match R, SPSS, SAS and MiniTab", {
  # Main table
  resultTable <- results$results$estimatesTable$data
  
  jaspTools::expect_equal_tables(
    "test"=resultTable,
    "ref"=list(3.75966210120454, "(Intercept)", 3.17912938532242e-21, 0.397567324274317,
               1, 89.4285652804951, 9.45666776832598, -0.0391768149758485,
               "Age", 2.69139209369272e-07, 0.0076162175667569, 1, 26.4593741560195,
               -5.14386762621471, -1.29196239983359, "PClass (2nd)", 6.77732368246485e-07,
               0.260075781079861, 1, 24.6774298532547, -4.96763825708504, -2.52141915270095,
               "PClass (3rd)", 7.94813113283019e-20, 0.276656805343585, 1,
               83.0629554227781, -9.11388805191166, -2.63135683476562, "Sex (male)",
               5.68409321426263e-39, 0.201505378991115, 1, 170.524272319145,
               -13.0584942592607))
})