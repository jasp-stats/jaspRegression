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

