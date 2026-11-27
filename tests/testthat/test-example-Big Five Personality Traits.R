context("Example: Big Five Personality Traits")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("Correlation results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "Big Five Personality Traits.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Run analysis
  set.seed(1)
  results <- jaspTools::runAnalysis("Correlation", dataset, opts)

  plotName <- results[["results"]][["corrPlot"]][["collection"]][["corrPlot_Agreeableness_Conscientiousness"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_agreeableness-vs-conscientiousness")

  plotName <- results[["results"]][["corrPlot"]][["collection"]][["corrPlot_Extraversion_Agreeableness"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_extraversion-vs-agreeableness")

  plotName <- results[["results"]][["corrPlot"]][["collection"]][["corrPlot_Extraversion_Conscientiousness"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-3_extraversion-vs-conscientiousness")

  plotName <- results[["results"]][["corrPlot"]][["collection"]][["corrPlot_Extraversion_Openness"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-4_extraversion-vs-openness")

  plotName <- results[["results"]][["corrPlot"]][["collection"]][["corrPlot_Neuroticism_Agreeableness"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-5_neuroticism-vs-agreeableness")

  plotName <- results[["results"]][["corrPlot"]][["collection"]][["corrPlot_Neuroticism_Conscientiousness"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-6_neuroticism-vs-conscientiousness")

  plotName <- results[["results"]][["corrPlot"]][["collection"]][["corrPlot_Neuroticism_Extraversion"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-7_neuroticism-vs-extraversion")

  plotName <- results[["results"]][["corrPlot"]][["collection"]][["corrPlot_Neuroticism_Openness"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-8_neuroticism-vs-openness")

  plotName <- results[["results"]][["corrPlot"]][["collection"]][["corrPlot_Openness_Agreeableness"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-9_openness-vs-agreeableness")

  plotName <- results[["results"]][["corrPlot"]][["collection"]][["corrPlot_Openness_Conscientiousness"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-10_openness-vs-conscientiousness")

  plotName <- results[["results"]][["heatmaps"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-11_pearson-s-r-heatmap")

  table <- results[["results"]][["mainTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.350078559679593, -0.424730517349808, 7.3229619211462e-16, -0.270698020840693,
     "-", "Neuroticism", "Extraversion", -0.010382646757846, -0.0979840238299593,
     0.816857869197202, 0.077378390847692, "-", "Neuroticism", "Openness",
     -0.134322225752612, -0.219428213473826, 0.00261532825667304,
     -0.0471874493961752, "-", "Neuroticism", "Agreeableness", -0.368006436237568,
     -0.441451087970854, 1.75902169561888e-17, -0.2896634924466,
     "-", "Neuroticism", "Conscientiousness", 0.267130892411439,
     0.183744494737788, 1.28782907428469e-09, 0.346700077567113,
     "-", "Extraversion", "Openness", 0.0545473493385421, -0.0333025339549289,
     0.223387557442278, 0.141560810698398, "-", "Extraversion", "Agreeableness",
     0.0645917390761422, -0.0232304277457905, 0.149244103107034,
     0.151424644277926, "-", "Extraversion", "Conscientiousness",
     0.159208473487536, 0.0725304901512851, 0.000351675993869984,
     0.243499543407598, "-", "Openness", "Agreeableness", -0.0134488495677684,
     -0.101020300182508, 0.764187600833046, 0.0743293975249047, "-",
     "Openness", "Conscientiousness", 0.158670001176117, 0.0719809467530214,
     0.000368493645824265, 0.242979800522451, "-", "Agreeableness",
     "Conscientiousness"))

})

