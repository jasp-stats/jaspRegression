context("Example: Fidgeting and Fat Gain")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("RegressionLinear (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "Fidgeting and Fat Gain.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("RegressionLinear", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_coeffTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("FALSE", 0.284733062124276, 1.78060584412937, "M<unicode>", "(Intercept)",
     4.80361922900816e-07, 8.38504661941205, 2.3875, 2.99439415587063,
     "TRUE", 0.303616402863274, 2.85393049653643, "M<unicode>", "(Intercept)",
     1.5344471311573e-08, 11.5445769153965, 3.50512291563107, 4.15631533472571,
     "FALSE", 0.00074140961470921, -0.00503165251004752, "M<unicode>",
     "jaspColumn1", 0.000380954307861458, -0.778555845705847, -4.64181603508707,
     -0.00344148703812493, -0.00185132156620235))

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_influenceTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 0.0518472644899152, 4.2, 3.82862269721482, 0.371377302785183,
     0.575255499203014, 2, 0.149892165577892, 3, 3.70128767680419,
     -0.701287676804193, -1.06566474780209, 3, 0.00235727315074185,
     3.7, 3.6049260397367, 0.0950739602633045, 0.142620732894912,
     4, 0.0128622331318761, 2.7, 3.04052216548421, -0.340522165484206,
     -0.484791407529147, 5, 0.00373716085901113, 3.2, 3.01299026917921,
     0.187009730820793, 0.265800567607384, 6, 0.038905886749189,
     3.6, 2.98545837287421, 0.614541627125793, 0.872083977087026,
     7, 0.00498053517319682, 2.4, 2.66195859129046, -0.261958591290464,
     -0.366932392357259, 8, 0.0638650625438183, 1.3, 2.28339501709672,
     -0.983395017096721, -1.37344080956228, 9, 0.19013896323741,
     3.8, 2.1560599966861, 1.6439400033139, 2.30043232938141, 10,
     0.00289777406468052, 1.7, 1.87729954659798, -0.177299546597979,
     -0.250466613319316, 11, 0.00527024451331424, 1.6, 1.83256021510235,
     -0.232560215102354, -0.329259403941504, 12, 0.0351771145453606,
     2.2, 1.66392735023423, 0.536072649765768, 0.766701161791654,
     13, 0.042776837546789, 1, 1.54003381686173, -0.540033816861735,
     -0.779602995699261, 14, 0.18899327853549, 0.4, 1.50906043351861,
     -1.10906043351861, -1.60521582913639, 15, 0.163585575948666,
     2.3, 1.37140095199361, 0.928599048006387, 1.36139318426987,
     16, 0.000258511655110305, 1.1, 1.13049685932487, -0.0304968593248676,
     -0.0459841947646068))

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 0, 1.1389322484971, 0, "M<unicode>", 0.778555845705847, 0.606149204882747,
     0.739852873694394, 0.578017005231515, "M<unicode>"))

})

