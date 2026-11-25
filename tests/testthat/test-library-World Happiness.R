context("Library: World Happiness")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/library/.

test_that("RegressionLinearBayesian results match", {

  # Run the R syntax of analysis 1 of the JASP example file on the data of the file
  jaspFile <- testthat::test_path("jaspfiles", "library", "World Happiness.jasp")
  set.seed(1)
  results <- jaspTools::runAnalysisSyntax(jaspFile = jaspFile, jaspRegression::RegressionLinearBayesian(
          data = NULL,
          version = "0.96.5",
          formula = `Happiness Score` ~ `GDP per Capita` + Family + `Life Expectancy` + Freedom + Generosity + `Government Corruption`,
          bayesFactorType = "BF01",
          modelPrior = "betaBinomial"))

  table <- results[["results"]][["basreg"]][["collection"]][["basreg_modelComparisonTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 2.97655354462899, "GDP per Capita + Family + Life Expectancy + Freedom + Generosity + Government Corruption",
     0.812424644427055, 0.331592022465011, 0.142857142857143, 0.178000847990578,
     18.4614770682163, "GDP per Capita + Family + Life Expectancy + Freedom + Government Corruption",
     0.810730828266772, 0.310477942669279, 0.0238095238095238, 0.106215615531765,
     27.3338748886507, "GDP per Capita + Family + Life Expectancy + Freedom",
     0.805632556524255, 0.208125092721321, 0.00952380952380953, 0.384696635588234,
     6.87815283617856, "GDP per Capita + Family + Life Expectancy + Freedom + Generosity",
     0.808732926483433, 0.143659527962849, 0.0238095238095238, 8.85472479006226,
     0.260289514600796, "Family + Life Expectancy + Freedom + Government Corruption",
     0.793653784866282, 0.00249653550563318, 0.00952380952380953,
     18.8049156587786, 0.122659321669565, "GDP per Capita + Family + Freedom",
     0.784282171926651, 0.000881663147237292, 0.00714285714285715,
     35.0548283150661, 0.0656254461356462, "GDP per Capita + Family + Freedom + Generosity",
     0.789777623911464, 0.000630615977699705, 0.00952380952380953,
     26.483088478856, 0.0870747466837511, "Family + Life Expectancy + Freedom",
     0.783288836107348, 0.00062604484882825, 0.00714285714285715,
     96.2758539430264, 0.0235487940055845, "Family + Life Expectancy + Freedom + Generosity + Government Corruption",
     0.793778711195387, 0.000574031128409482, 0.0238095238095238,
     62.36548557403, 0.036877019550508, "GDP per Capita + Family + Freedom + Government Corruption",
     0.788132970491334, 0.000354461039267625, 0.00952380952380953
    ))

})

