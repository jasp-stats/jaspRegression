context("Example: Stepwise")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("RegressionLinear (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "Stepwise.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("RegressionLinear", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_anovaTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("FALSE", 111.479171821261, 666.974859393145, 2667.89943757258,
     "Regression", 4, "M<unicode>", 4.7561817455974e-07, "FALSE",
     5.98295491881238, 47.863639350499, "Residual", 8, "M<unicode>",
     "FALSE", 2715.76307692308, "Total", 12, "M<unicode>", "TRUE",
     166.831680052448, 889.26344917423, 2667.79034752269, "Regression",
     3, "M<unicode>", 3.32337479333999e-08, "FALSE", 5.33030326670968,
     47.9727294003871, "Residual", 9, "M<unicode>", "FALSE", 2715.76307692308,
     "Total", 12, "M<unicode>", "TRUE", 229.503697119894, 1328.92929687348,
     2657.85859374696, "Regression", 2, "M<unicode>", 4.40657890746393e-09,
     "FALSE", 5.79044831761138, 57.9044831761138, "Residual", 10,
     "M<unicode>", "FALSE", 2715.76307692308, "Total", 12, "M<unicode>"
    ))

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_coeffTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("FALSE", 70.0709592085348, "M<unicode>", "(Intercept)", 0.399133563385553,
     0.89060246933678, 62.4053692999179, "FALSE", 0.744769867130978,
     "M<unicode>", "jaspColumn1", 0.0708216874297203, 0.606511951747821,
     2.08266031691594, 1.55110264750845, "FALSE", 0.723788001835165,
     "M<unicode>", "jaspColumn2", 0.50090110347428, 0.527705631163759,
     0.704857746178968, 0.510167579684914, "FALSE", 0.754709045051294,
     "M<unicode>", "jaspColumn3", 0.895922690510105, 0.043389698365702,
     0.135031379639467, 0.101909403579661, "FALSE", 0.709052063446495,
     "M<unicode>", "jaspColumn4", 0.844071473291883, -0.160287415965402,
     -0.203174120065002, -0.144061029071016, "TRUE", 14.1423934847943,
     "M<unicode>", "(Intercept)", 0.000675332164159644, 5.06620799735702,
     71.6483069744349, "FALSE", 0.116997594968193, "M<unicode>",
     "jaspColumn1", 5.78076367351318e-07, 0.567736589958952, 12.4099812771581,
     1.4519379630278, "FALSE", 0.185610487002054, "M<unicode>", "jaspColumn2",
     0.0516873489774238, 0.430414384028907, 2.24184402868526, 0.416109761946919,
     "FALSE", 0.173287794991198, "M<unicode>", "jaspColumn4", 0.205395438101685,
     -0.263183042389062, -1.36501370769238, -0.236540215538773, "TRUE",
     2.28617433450336, "M<unicode>", "(Intercept)", 5.45657090149133e-10,
     22.997961305305, 52.5773488820895, "FALSE", 0.121300923606267,
     "M<unicode>", "jaspColumn1", 2.69221217968561e-07, 0.57413671680864,
     12.1046542644767, 1.46830574221555, "FALSE", 0.0458547214685228,
     "M<unicode>", "jaspColumn2", 5.02896031563878e-08, 0.685016703143769,
     14.4423620963274, 0.662250491274645))

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.991148636889382, 0.98237562040768, 0.98237562040768, 2.44600795559057,
     0.97356343061152, 4, 8, "M<unicode>", 4.75618174559735e-07,
     0.99112837271487, 0.982335451200427, -4.01692072533599e-05,
     2.30874495488559, 0.976447268267236, 1, 9, "M<unicode>", 1,
     0.9892817467919, 0.978678374535632, -0.00365707666479487, 2.40633503852048,
     0.974414049442758, 1, 10, "M<unicode>", 1))

})

test_that("RegressionLinear (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "Stepwise.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("RegressionLinear", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_anovaTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", 22.7985202013823, 1831.8961600238, 1831.8961600238, "Regression",
     1, "M<unicode>", 0.000576231816488498, "FALSE", 80.3515378999347,
     883.866916899282, "Residual", 11, "M<unicode>", "FALSE", 2715.76307692308,
     "Total", 12, "M<unicode>", "TRUE", 176.626963081888, 1320.50048238317,
     2641.00096476634, "Regression", 2, "M<unicode>", 1.58106023181438e-08,
     "FALSE", 7.47621121567355, 74.7621121567355, "Residual", 10,
     "M<unicode>", "FALSE", 2715.76307692308, "Total", 12, "M<unicode>"
    ))

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_coeffTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("FALSE", 4.17237793980649, "M<unicode>", "(Intercept)", 2.89915283322214e-11,
     22.8701901648686, 95.4230769230769, "TRUE", 5.26220651126062,
     "M<unicode>", "(Intercept)", 1.62423647336317e-10, 22.341945517515,
     117.567931176498, "FALSE", 0.154595996197937, "M<unicode>",
     "jaspColumn4", 0.000576231816488499, -0.821305037200923, -4.77477959715234,
     -0.738161808447353, "TRUE", 2.12398360630339, "M<unicode>",
     "(Intercept)", 3.32433765781353e-13, 48.5396315351542, 103.097381636675,
     "FALSE", 0.0486445523855851, "M<unicode>", "jaspColumn4", 1.81489046525989e-07,
     -0.683106605512828, -12.6212206279072, -0.613953628004261, "FALSE",
     0.138416639790684, "M<unicode>", "jaspColumn1", 1.10528141953732e-06,
     0.563052297843079, 10.4030721102348, 1.43995828499888))

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 0, 0, 15.0437226025871, 0, 0, 12, "M<unicode>", "", 0.821305037200923,
     0.67454196413161, 0.67454196413161, 8.96390193497981, 0.644954869961756,
     1, 11, "M<unicode>", 0.000576231816488497, 0.986139466666319,
     0.972471047716931, 0.297929083585322, 2.73426612012685, 0.966965257260318,
     1, 10, "M<unicode>", 1.10528141953731e-06))

})

test_that("RegressionLinear (analysis 3) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "Stepwise.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[3]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("RegressionLinear", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_anovaTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", 22.7985202013823, 1831.8961600238, 1831.8961600238, "Regression",
     1, "M<unicode>", 0.000576231816488498, "FALSE", 80.3515378999347,
     883.866916899282, "Residual", 11, "M<unicode>", "FALSE", 2715.76307692308,
     "Total", 12, "M<unicode>", "TRUE", 176.626963081888, 1320.50048238317,
     2641.00096476634, "Regression", 2, "M<unicode>", 1.58106023181438e-08,
     "FALSE", 7.47621121567355, 74.7621121567355, "Residual", 10,
     "M<unicode>", "FALSE", 2715.76307692308, "Total", 12, "M<unicode>"
    ))

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_coeffTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("FALSE", 4.17237793980649, "M<unicode>", "(Intercept)", 2.89915283322214e-11,
     22.8701901648686, 95.4230769230769, "TRUE", 5.26220651126062,
     "M<unicode>", "(Intercept)", 1.62423647336317e-10, 22.341945517515,
     117.567931176498, "FALSE", 0.154595996197937, "M<unicode>",
     "jaspColumn4", 0.000576231816488499, -0.821305037200923, -4.77477959715234,
     -0.738161808447353, "TRUE", 2.12398360630339, "M<unicode>",
     "(Intercept)", 3.32433765781353e-13, 48.5396315351542, 103.097381636675,
     "FALSE", 0.0486445523855851, "M<unicode>", "jaspColumn4", 1.81489046525989e-07,
     -0.683106605512828, -12.6212206279072, -0.613953628004261, "FALSE",
     0.138416639790684, "M<unicode>", "jaspColumn1", 1.10528141953732e-06,
     0.563052297843079, 10.4030721102348, 1.43995828499888))

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 0, 0, 15.0437226025871, 0, 0, 12, "M<unicode>", "", 0.821305037200923,
     0.67454196413161, 0.67454196413161, 8.96390193497981, 0.644954869961756,
     1, 11, "M<unicode>", 0.000576231816488497, 0.986139466666319,
     0.972471047716931, 0.297929083585322, 2.73426612012685, 0.966965257260318,
     1, 10, "M<unicode>", 1.10528141953731e-06))

})

test_that("RegressionLinear (analysis 4) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "Stepwise.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[4]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("RegressionLinear", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_anovaTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("FALSE", 32.2682210635856, 309.420267397843, 2475.36213918274,
     "Regression", 8, "M<unicode>", 0.0079145674979206, "FALSE",
     9.58900916130827, 28.7670274839248, "Residual", 3, "M<unicode>",
     "FALSE", 2504.12916666667, "Total", 11, "M<unicode>", "TRUE",
     70.2126970288405, 412.459503610916, 2474.75702166549, "Regression",
     6, "M<unicode>", 0.000115383255685847, "FALSE", 5.87442900023473,
     29.3721450011736, "Residual", 5, "M<unicode>", "FALSE", 2504.12916666667,
     "Total", 11, "M<unicode>", "TRUE", 91.7674559374213, 494.361302342999,
     2471.806511715, "Regression", 5, "M<unicode>", 1.39086201294219e-05,
     "FALSE", 5.38710915861193, 32.3226549516716, "Residual", 6,
     "M<unicode>", "FALSE", 2504.12916666667, "Total", 11, "M<unicode>",
     "TRUE", 50.231465012238, 604.956384895691, 2419.82553958277,
     "Regression", 4, "M<unicode>", 3.06798013194108e-05, "FALSE",
     12.0433752977004, 84.303627083903, "Residual", 7, "M<unicode>",
     "FALSE", 2504.12916666667, "Total", 11, "M<unicode>", "TRUE",
     75.6404477726701, 806.284557937604, 2418.85367381281, "Regression",
     3, "M<unicode>", 3.2641567600042e-06, "FALSE", 10.6594366067321,
     85.2754928538564, "Residual", 8, "M<unicode>", "FALSE", 2504.12916666667,
     "Total", 11, "M<unicode>"))

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_coeffTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("FALSE", 133.421504516949, "M<unicode>", "(Intercept)", 0.96438582070519,
     -0.0484730237427266, -6.46734375624036, "FALSE", 1.42963125555783,
     "M<unicode>", "jaspColumn2", 0.537104650373014, 0.98844884899126,
     0.694896726043951, 0.993446078937239, "FALSE", 2.46309986283759,
     "M<unicode>", "jaspColumn1", 0.60249419505702, 0.577011334005119,
     0.58018646158705, 1.42905719395529, "FALSE", 4.33773809650006,
     "M<unicode>", "jaspColumn6 (W)", 0.495546730437596, -0.773575776157231,
     -3.35556911476682, "FALSE", 4.48435449945435, "M<unicode>",
     "jaspColumn3", 0.927930236235576, 0.192276565713494, 0.0982502202390285,
     0.440588817201269, "FALSE", 1.26012845498367, "M<unicode>",
     "jaspColumn4", 0.638865026875324, 0.718828316261768, 0.52021556670802,
     0.655538438334229, "FALSE", 0.0632586140473492, "M<unicode>",
     "jaspColumn2 <unicode><unicode><unicode> jaspColumn3", 0.785040661685271,
     0.417417918789843, 0.298157165068561, 0.0188610090305239, "FALSE",
     0.0691775857485198, "M<unicode>", "jaspColumn3 <unicode><unicode><unicode> jaspColumn4",
     0.997317377339969, 0.00507013053322872, 0.00364931525633705,
     0.000252450819068638, "FALSE", 0.0358137589704193, "M<unicode>",
     "jaspColumn2 <unicode><unicode><unicode> jaspColumn1", 0.515141336043213,
     0.549692921917793, 0.735837632320596, 0.0263531116052938, "TRUE",
     44.4001189488228, "M<unicode>", "(Intercept)", 0.625990758073355,
     0.518869541517009, 23.0378693622764, "FALSE", 0.322142852095929,
     "M<unicode>", "jaspColumn2", 0.0964751533389187, 0.654890189127081,
     2.04319625967059, 0.658201070482018, "FALSE", 0.596912863529389,
     "M<unicode>", "jaspColumn1", 0.17522073840756, 0.380509451690811,
     1.57877331606735, 0.942390100957553, "FALSE", 2.75641476085485,
     "M<unicode>", "jaspColumn6 (W)", 0.367596289643325, -0.990082854460647,
     -2.72907899450463, "FALSE", 0.556882161932982, "M<unicode>",
     "jaspColumn4", 0.5101728050101, 0.432768889870146, 0.708705351577746,
     0.39466536836009, "FALSE", 0.0178480678720889, "M<unicode>",
     "jaspColumn2 <unicode><unicode><unicode> jaspColumn3", 0.280087853063952,
     0.478253805910225, 1.2107683522938, 0.0216098757291171, "FALSE",
     0.021739455251179, "M<unicode>", "jaspColumn2 <unicode><unicode><unicode> jaspColumn1",
     0.221599843454666, 0.632919200326543, 1.39576199822874, 0.0303431055017898,
     "TRUE", 3.77998920386946, "M<unicode>", "(Intercept)", 7.06360421720896e-06,
     14.3862515421814, 54.379875513596, "FALSE", 0.147316537937606,
     "M<unicode>", "jaspColumn2", 0.0209472605538291, 0.455308454682427,
     3.10630653818397, 0.457610324978212, "FALSE", 0.565500790637357,
     "M<unicode>", "jaspColumn1", 0.126139153822249, 0.40543184168006,
     1.77561949392804, 1.00411422768741, "FALSE", 2.4260258648231,
     "M<unicode>", "jaspColumn6 (W)", 0.450136146800315, -0.80761332854038,
     -1.95929082381484, "FALSE", 0.007655496636379, "M<unicode>",
     "jaspColumn2 <unicode><unicode><unicode> jaspColumn3", 0.227075131582359,
     0.22796597873561, 1.34552107039256, 0.0103006320285673, "FALSE",
     0.013521483034569, "M<unicode>", "jaspColumn2 <unicode><unicode><unicode> jaspColumn1",
     0.217480442729127, 0.388563923862179, 1.37768492097394, 0.0186283432859306,
     "TRUE", 4.62244185571477, "M<unicode>", "(Intercept)", 3.3018228737537e-06,
     13.2259388833089, 61.1361334753326, "FALSE", 0.755307665598902,
     "M<unicode>", "jaspColumn1", 0.784574004021762, 0.0866338587581169,
     0.284072395130715, 0.214562057627269, "FALSE", 2.94600116228339,
     "M<unicode>", "jaspColumn6 (W)", 0.06785617585389, -2.15751785381535,
     -6.35605010498718, "FALSE", 0.00664933804268056, "M<unicode>",
     "jaspColumn2 <unicode><unicode><unicode> jaspColumn3", 0.00293552287395333,
     0.656347851461263, 4.46015026032893, 0.0296570468020768, "FALSE",
     0.0124344371027636, "M<unicode>", "jaspColumn1 <unicode><unicode><unicode> jaspColumn2",
     0.00423355106000876, 1.0793681352717, 4.16155098154541, 0.0517465439299706,
     "TRUE", 2.94241587373803, "M<unicode>", "(Intercept)", 2.66756942724447e-08,
     21.1061352447325, 62.1030273773627, "FALSE", 2.6775112603785,
     "M<unicode>", "jaspColumn6 (W)", 0.0510150762342516, -2.29312896423711,
     -6.13987862324493, "FALSE", 0.00464824917532219, "M<unicode>",
     "jaspColumn2 <unicode><unicode><unicode> jaspColumn3", 0.0002868346983257,
     0.62837138083493, 6.10830663795317, 0.0283929312924809, "FALSE",
     0.00377490003234752, "M<unicode>", "jaspColumn2 <unicode><unicode><unicode> jaspColumn1",
     4.76670821753632e-07, 1.14910558993589, 14.5937277027167, 0.0550898631770563
    ))

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.994239489803004, 0.988512163083737, 0.988512163083737, 3.09661253005736,
     0.957877931307035, 8, 3, "M<unicode>", 0.00791456749792058,
     0.994117958392561, 0.988270515198594, -0.000241647885142582,
     2.42372213758812, 0.974195133436907, 2, 5, "M<unicode>", 1,
     0.993525166922146, 0.987092257307678, -0.00117825789091608,
     2.32101468298068, 0.976335805064077, 1, 6, "M<unicode>", 1,
     0.983022967138719, 0.966334153922211, -0.0207581033854675, 3.47035665280968,
     0.947096527592045, 1, 7, "M<unicode>", 1, 0.982825543336556,
     0.965946048634796, -0.000388105287415064, 3.26487926372968,
     0.953175816872844, 1, 8, "M<unicode>", 1))

})

