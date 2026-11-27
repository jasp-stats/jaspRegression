context("Example: World Happiness")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("RegressionLinearBayesian results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "World Happiness.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Run analysis
  set.seed(1)
  results <- jaspTools::runAnalysis("RegressionLinearBayesian", dataset, opts)

  table <- results[["results"]][["basreg"]][["collection"]][["basreg_modelComparisonTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 2.97655354462895, "GDP per Capita + Family + Life Expectancy + Freedom + Generosity + Government Corruption",
     0.812424644427055, 0.331592022465008, 0.142857142857143, 0.178000847990573,
     18.4614770682168, "GDP per Capita + Family + Life Expectancy + Freedom + Government Corruption",
     0.810730828266772, 0.310477942669285, 0.0238095238095238, 0.106215615531765,
     27.3338748886504, "GDP per Capita + Family + Life Expectancy + Freedom",
     0.805632556524255, 0.208125092721319, 0.00952380952380953, 0.384696635588234,
     6.87815283617848, "GDP per Capita + Family + Life Expectancy + Freedom + Generosity",
     0.808732926483433, 0.143659527962848, 0.0238095238095238, 8.854724790062,
     0.260289514600801, "Family + Life Expectancy + Freedom + Government Corruption",
     0.793653784866282, 0.00249653550563323, 0.00952380952380953,
     18.8049156587786, 0.122659321669564, "GDP per Capita + Family + Freedom",
     0.784282171926651, 0.000881663147237284, 0.00714285714285715,
     35.0548283150656, 0.0656254461356465, "GDP per Capita + Family + Freedom + Generosity",
     0.789777623911464, 0.000630615977699709, 0.00952380952380953,
     26.4830884788541, 0.0870747466837566, "Family + Life Expectancy + Freedom",
     0.783288836107348, 0.000626044848828289, 0.00714285714285715,
     96.2758539430195, 0.0235487940055859, "Family + Life Expectancy + Freedom + Generosity + Government Corruption",
     0.793778711195387, 0.000574031128409517, 0.0238095238095238,
     62.36548557403, 0.0368770195505077, "GDP per Capita + Family + Freedom + Government Corruption",
     0.788132970491334, 0.000354461039267622, 0.00952380952380953
    ))

})

