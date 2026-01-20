context("Linear Regression -- Verification project")

# Some of the sample files used in this file below are distributed under the following license restrictions:
# This data set comes from Field, A. P. (2017). {Discovering Statistics Using IBM SPSS Statistics} (5th ed.). London: Sage. The data set was constructed by Andy Field who therefore owns the copyright. Andy Field generously agreed that we can include the data set in the JASP data library. This data set is also publicly available on the website that accompanies Andy Field`s book, {https://edge.sagepub.com/field5e}. Without Andy Field`s explicit consent, this data set may not be distributed for commercial purposes, this data set may not be edited, and this data set may not be presented without acknowledging its source (i.e., the terms of a CC BY-NC-ND license see https://creativecommons.org/licenses/by-nc-nd/3.0/).

# does not test
# - stepwise methods (currently gives an error if I set p entry too high)
# - plots handle errors

## Testing linear regression standard

options <- jaspTools::analysisOptions("RegressionLinear")
options$dependent <- "Y"
options$covariates <- "MeanCenteredX"

options$modelTerms <- list(
  list(components=NULL,  name="model0", title = "Model 0"),
  list(components=list("MeanCenteredX"),  name="model1", title = "Model 1")
)
options$descriptives <- TRUE
options$residualDurbinWatson <- FALSE
options$residualCasewiseDiagnostic <- FALSE
options$residualsSavedToData <- FALSE
options$residualsSavedToDataColumn <- FALSE
options$residualStatistic <- FALSE
options$rSquaredChange <- FALSE

results <- jaspTools::runAnalysis("RegressionLinear", test_path("Regression.csv"), options)

# https://jasp-stats.github.io/jasp-verification-project/regression.html#linear-regression
test_that("Main table results match R, SPSS, SAS and MiniTab", {
  # Main table
  resultTable <- results$results$modelContainer$collection$modelContainer_summaryTable$data

  jaspTools::expect_equal_tables(
    "test"=resultTable,
    "ref"=list(0, 0, 11.711593246071, 0, "H<unicode>", 0.806436535963374, 0.650339886536607,
            6.96410449545021, 0.646411121216794, "H<unicode>")
  )
})

# https://jasp-stats.github.io/jasp-verification-project/regression.html#linear-regression
test_that("ANOVA table results match R, SPSS, SAS and MiniTab", {
  # ANOVA table
  resultTable <- results$results$modelContainer$collection$modelContainer_anovaTable$data

  jaspTools::expect_equal_tables(
    "test"=resultTable,
    "ref"=list("TRUE", 165.532892294899, 8028.13859583154, 8028.13859583154,
               "Regression", 1, "H<unicode>", 5.11889396069713e-22, "FALSE",
               48.4987514235498, 4316.38887669593, "Residual", 89, "H<unicode>",
               "FALSE", 12344.5274725275, "Total", 90, "H<unicode>")
  )
})

# https://jasp-stats.github.io/jasp-verification-project/regression.html#linear-regression
test_that("Coefficient table results match R, SPSS, SAS and MiniTab", {
  # Coefficient table
  resultTable <- results$results$modelContainer$collection$modelContainer_coeffTable$data

  jaspTools::expect_equal_tables(
    "test"=resultTable,
    "ref"=list("FALSE", 1.22770856137111, "H<unicode>", "(Intercept)", 6.22465705469213e-34,
               19.4143509177338, 23.8351648351648, "TRUE", 0.73003651439274,
               "H<unicode>", "(Intercept)", 2.54923669436547e-51, 32.6492776255054,
               23.8351648351648, "FALSE", 0.419657937490671, "H<unicode>",
               "MeanCenteredX", 5.11889396069709e-22, 0.806436535963375, 12.8659586621013,
               5.39930167597765)
  )
})

# https://jasp-stats.github.io/jasp-verification-project/regression.html#linear-regression
test_that("Descriptive table results match R, SPSS, SAS and MiniTab", {
  # Descriptives table
  resultTable <- results$results$modelContainer$collection$modelContainer_descriptivesTable$data

  jaspTools::expect_equal_tables(
    "test"=resultTable,
    "ref"=list(91, 11.711593246071, 1.22770856137111, 23.8351648351648, "Y",
               91, 1.74923670777544, 0.183369831659836, 1.14617087951332e-15,
               "MeanCenteredX")
  )
})
