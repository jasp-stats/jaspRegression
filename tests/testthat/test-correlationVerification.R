context("Correlation -- Verification project")

# https://jasp-stats.github.io/jasp-verification-project/regression.html#correlation
test_that("Correlation Table results match R, SPSS, SAS and MiniTab", {
  options <- jaspTools::analysisOptions("Correlation")
  options$variables <- c("X", "Y")
  options$pearson <- TRUE
  options$kendallsTauB <- TRUE
  options$spearman <- TRUE
  options$ci <- TRUE
  options$significanceFlagged <- FALSE

  results <- jaspTools::runAnalysis("Correlation", "Correlation.csv", options)

  # Main table
  resultTable <- results$results$mainTable$data

  jaspTools::expect_equal_tables(
    "test"=resultTable,
    "ref"=list("<unicode>", "<unicode>", "<unicode>", "<unicode>", "<unicode>",
              "<unicode>", "<unicode>", "<unicode>", "<unicode>", "<unicode>",
              "<unicode>", "<unicode>", 0.612084493088406, 0.517248967986794,
              3.58243017602502e-16, 0.706920018190018, 0.806436535963374,
              0.720084338869883, 5.11889396069723e-22, 0.868190969147739,
              0.773033328906496, 0.674464778571329, 2.77257960858756e-19,
              0.844527512492866, "X", "", "", "", "", "", "", "", "", "",
              "", "", "", "<unicode>", "<unicode>", "<unicode>", "<unicode>",
              "<unicode>", "<unicode>", "<unicode>", "<unicode>", "<unicode>",
              "<unicode>", "<unicode>", "<unicode>", "Y"))
})
