context("Generalized Linear Model (unit)")

# Unit tests for internal helper functions. These call the functions directly
# so they stay fast and do not require runAnalysis.

test_that("Test that .glmCheckDataErrors requires a positive dependent variable for the Gamma and Inverse Gaussian families", {
  options <- list(dependent = "y", covariates = list(), factors = list(), weights = "", interceptTerm = TRUE)

  for (family in c("gamma", "inverseGaussian")) {
    options$family <- family
    expect_error(
      jaspRegression:::.glmCheckDataErrors(data.frame(y = c(1.2, 0, 3.4)), options),
      "require the dependent variable to be positive",
      class = "validationError"
    )
    expect_no_error(jaspRegression:::.glmCheckDataErrors(data.frame(y = c(1.2, 0.5, 3.4)), options))
  }
})
