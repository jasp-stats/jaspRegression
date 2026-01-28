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
options$oddsRatio <- FALSE
options$coefficientCiAsOddsRatio <- FALSE

results <- jaspTools::runAnalysis("RegressionLogistic", dataset = testthat::test_path("LogReg.csv"), options)

# https://jasp-stats.github.io/jasp-verification-project/regression.html#logistic-regression
test_that("Main table results match R, SPSS, SAS and MiniTab", {
  # Main table
  resultTable <- results$results$modelSummary$data

  jaspTools::expect_equal_tables(
    "test"=resultTable,
    "ref"=list(1027.57254724532, 1032.2005886215, 1025.57254724532, 755, 0, "M<unicode>",
               0, 705.140778094396, 728.280984975294, 0.354079636893343, 330.431769150921,
               695.140778094396, 751, 0.322192486566122, "M<unicode>", 0.476901084612001,
               0, 0.393957934297112)
  )
})

# https://jasp-stats.github.io/jasp-verification-project/regression.html#logistic-regression
test_that("Estimates table results match R, SPSS, SAS and MiniTab", {
  # Main table
  resultTable <- results$results$estimatesTable$data

  jaspTools::expect_equal_tables(
    "test"=resultTable,
    "ref"=list("TRUE", -0.347366579504982, "M<unicode>", "(Intercept)", 2.54655183336123e-06,
               0.0738391799970593, 1, 22.1310660444386, -4.70436669961416,
               "TRUE", 3.75966210120455, "M<unicode>", "(Intercept)", 3.1791293853215e-21,
               0.397567324274317, 1, 89.4285652804962, 9.45666776832601, "FALSE",
               -0.0391768149758488, "M<unicode>", "Age", 2.6913920936924e-07,
               0.00761621756675692, 1, 26.45937415602, -5.14386762621473, "FALSE",
               -1.2919623998336, "M<unicode>", "PClass (2nd)", 6.77732368246352e-07,
               0.260075781079861, 1, 24.6774298532553, -4.96763825708507, "FALSE",
               -2.52141915270095, "M<unicode>", "PClass (3rd)", 7.94813113282811e-20,
               0.276656805343584, 1, 83.0629554227789, -9.11388805191169, "FALSE",
               -2.63135683476561, "M<unicode>", "Sex (male)", 5.68409321426302e-39,
               0.201505378991114, 1, 170.524272319145, -13.0584942592607))
})
