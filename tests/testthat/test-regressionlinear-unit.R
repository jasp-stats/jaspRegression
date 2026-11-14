context("Linear Regression (unit)")

# Unit tests for the descriptives-plots helper functions. These call the
# internal functions directly so they stay fast and do not require runAnalysis.

test_that("Test that .linregSdGroupLabels returns the expected labels", {
  expect_identical(jaspRegression:::.linregSdGroupLabels(2), c("Low", "High"))
  expect_identical(jaspRegression:::.linregSdGroupLabels(3), c("Low", "Medium", "High"))
  expect_identical(jaspRegression:::.linregSdGroupLabels(4), c("Low", "Med-Low", "Med-High", "High"))
  expect_identical(jaspRegression:::.linregSdGroupLabels(5), c("Very Low", "Low", "Medium", "High", "Very High"))
  # falls back to generic G1..Gn for group counts without a named label set
  expect_identical(jaspRegression:::.linregSdGroupLabels(6), paste0("G", 1:6))
})

test_that("Test that .linregBinScaleVariable bins by standard deviation", {
  x      <- seq(0, 100, by = 1) # mean 50, breaks at mean +/- 1 SD
  binned <- jaspRegression:::.linregBinScaleVariable(x, method = "sd", nGroups = 3)
  binned2 <- jaspRegression:::.linregBinScaleVariable(x, method = "sd", nGroups = 2)

  expect_s3_class(binned, "factor")
  expect_identical(levels(binned), c("Low", "Medium", "High"))
  expect_identical(as.character(binned[x ==   0]), "Low")
  expect_identical(as.character(binned[x ==  50]), "Medium")
  expect_identical(as.character(binned[x == 100]), "High")

  expect_s3_class(binned2, "factor")
  expect_identical(levels(binned2), c("Low", "High"))
  expect_identical(as.character(binned2[x ==   0]), "Low")
  expect_identical(as.character(binned2[x ==  50]), "Low")
  expect_identical(as.character(binned2[x == 100]), "High")
})


test_that("Test that .linregBinScaleVariable bins by percentile", {
  x      <- 1:100
  binned <- jaspRegression:::.linregBinScaleVariable(x, method = "percentile", nGroups = 4)

  expect_s3_class(binned, "factor")
  expect_identical(levels(binned), c("Q1", "Q2", "Q3", "Q4"))
  # percentile groups are equal-frequency
  expect_equal(as.vector(table(binned)), rep(25L, 4))
})

test_that("Test that .linregBinScaleVariable handles zero-variance input", {
  binned <- jaspRegression:::.linregBinScaleVariable(rep(5, 10), method = "sd", nGroups = 3)
  expect_identical(nlevels(binned), 1L)
})

test_that("Test that .linregCustomGroupLabels extracts one label per row", {
  rows <- list(list(label = "Lo", value = "1"), list(label = "", value = "2"), list(value = "3"))
  expect_identical(jaspRegression:::.linregCustomGroupLabels(rows), c("Lo", "", ""))
  expect_identical(jaspRegression:::.linregCustomGroupLabels(NULL), character(0))
})

test_that("Test that .linregResolveGroupLabels falls back to the defaults", {
  defaults <- c("Low", "Medium", "High")
  # empty and whitespace-only labels keep the default
  expect_identical(jaspRegression:::.linregResolveGroupLabels(c(" Lo ", "", "  "), defaults), c("Lo", "Medium", "High"))
  # a label count that does not match the bins is ignored
  expect_identical(jaspRegression:::.linregResolveGroupLabels(c("Lo", "Hi"), defaults), defaults)
  expect_identical(jaspRegression:::.linregResolveGroupLabels(character(0), defaults), defaults)
})

test_that("Test that .linregBinScaleVariable uses custom labels", {
  x <- seq(0, 100, by = 1)

  binned <- jaspRegression:::.linregBinScaleVariable(x, method = "sd", nGroups = 3, labels = c("Lo", "", "Hi"))
  expect_identical(levels(binned), c("Lo", "Medium", "Hi"))
  expect_identical(as.character(binned[x == 100]), "Hi")

  binned2 <- jaspRegression:::.linregBinScaleVariable(1:100, method = "percentile", nGroups = 2, labels = c("Bottom", "Top"))
  expect_identical(levels(binned2), c("Bottom", "Top"))

  # ties merge percentile bins, so the custom labels no longer match and the defaults are used
  tied    <- c(rep(1, 60), 2:41) # quartile cut-points 1, 1, 1, 16.25, 41 -> 2 bins
  binned3 <- jaspRegression:::.linregBinScaleVariable(tied, method = "percentile", nGroups = 4, labels = c("A", "B", "C", "D"))
  expect_identical(levels(binned3), c("Q1", "Q2"))
})
