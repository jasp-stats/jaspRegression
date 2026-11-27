initClassicalRegressionOptions <- function(analysis = c("RegressionLinear", "RegressionLogistic")) {
  analysis <- match.arg(analysis)
  options <- c(
    jaspTools::analysisOptions(analysis),
    classicalRegressionCommonOptions()
  )

  return(options)
}

classicalRegressionCommonOptions <- function() {
  path <- testthat::test_path("..", "..", "inst", "qml", "common")
  files <- list.files(path, full.names = TRUE)
  options <- lapply(files, jaspTools:::readQML) |>
    lapply(function(x) {x$plotWidth <- NULL; x$plotHeight <- NULL; return(x)}) |>
    (function(x) { do.call(c, x)})()

  return(options)
}

# test.csv (or debug.csv) plus two columns that still reach the analyses' own data checks. The QML form refuses
# a variable with fewer than 2 distinct numeric values (1 for a dependent variable) before the analysis runs,
# so debInf (Inf throughout), debSame, debMiss99 and debNaN never get there.
#  - `contInf`:    contGamma with one infinite value
#  - `contTwoObs`: contNormal with only two observed values
testDataWithCheckColumns <- function(file = "test.csv") {
  dataset <- jaspTools:::loadCorrectDataset(file)
  dataset[["contInf"]] <- dataset[["contGamma"]]
  dataset[["contInf"]][1] <- Inf
  dataset[["contTwoObs"]] <- NA_real_
  dataset[["contTwoObs"]][1:2] <- dataset[["contNormal"]][1:2]
  dataset
}

