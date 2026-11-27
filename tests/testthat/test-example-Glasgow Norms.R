context("Example: Glasgow Norms")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("CorrelationBayesian results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "Glasgow Norms.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Run analysis
  set.seed(1)
  results <- jaspTools::runAnalysis("CorrelationBayesian", dataset, opts)

  table <- results[["results"]][["corBayesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", "<unicode>", "Pearson's r", "1. M AROU", "FALSE", "<unicode>",
     "BF<unicode><unicode>", "", "FALSE", "<unicode>", "Upper 95% CI",
     "", "FALSE", "<unicode>", "Lower 95% CI", "", "TRUE", 0.317034386114287,
     "<unicode>", "Pearson's r", "2. M VAL", "FALSE", 7.31210394255761e+125,
     "<unicode>", "BF<unicode><unicode>", "", "FALSE", 0.340414744670677,
     "<unicode>", "Upper 95% CI", "", "FALSE", 0.293100168727737,
     "<unicode>", "Lower 95% CI", "", 0, "TRUE", 0.347292142203392,
     "<unicode>", 0.710030680865603, "Pearson's r", "3. M DOM", "FALSE",
     1.24937976538968e+153, "<unicode>", "<unicode>", "BF<unicode><unicode>",
     "", "FALSE", 0.370125010967267, "<unicode>", "", "Upper 95% CI",
     "", "FALSE", 0.323866089800627, "<unicode>", "", "Lower 95% CI",
     "", "TRUE", -0.264966534120283, "<unicode>", 0.0391454758966565,
     0.0563288554511697, "Pearson's r", "4. M CNC", "FALSE", 8.67823263444173e+85,
     "<unicode>", 1.18473293817397, 113.52463136621, "BF<unicode><unicode>",
     "", "FALSE", -0.240274355237224, "<unicode>", 0.0653632996534428,
     0.0824870482477849, "Upper 95% CI", "", "FALSE", -0.289180249768599,
     "<unicode>", 0.0128517542864677, 0.0300616276254229, "Lower 95% CI",
     "", 0, "TRUE", -0.101350145944514, 0.908251583996921, 0.0694640934562379,
     "<unicode>", 0.0909180351296638, "Pearson's r", "5. M IMAG",
     "FALSE", 46436035825.3604, "<unicode>", 11325.2712056632, "<unicode>",
     168810006.19827, "BF<unicode><unicode>", "", "FALSE", -0.0752263797864292,
     "", 0.0955663136095319, "<unicode>", 0.116909511127017, "Upper 95% CI",
     "", "FALSE", -0.12727912439847, "", 0.0432276350173366, "<unicode>",
     0.0647514677525372, "Lower 95% CI", "", "TRUE", 0.189292964841549,
     0.0924104875061614, 0.191930721545424, "<unicode>", 0.210418382849168,
     0.245715550766478, "Pearson's r", "6. M FAM", "FALSE", 1.56267554264961e+42,
     362975534.588783, 2.82632940282763e+43, "<unicode>", 6.21099982811964e+52,
     1.83998188549447e+73, "BF<unicode><unicode>", "", "FALSE", 0.2144711636082,
     0.118393368954689, 0.217080239916735, "<unicode>", 0.235356852561227,
     0.2702024166554, "Upper 95% CI", "", "FALSE", 0.16376033654918,
     0.0662496894946916, 0.166422208855497, "<unicode>", 0.185089372785245,
     0.220780301139872, "Lower 95% CI", "", 0, 0, "TRUE", "<unicode>",
     -9.88699815533011e-05, -0.367055605434119, -0.129408034367433,
     -0.67226489778625, -0.476960453584576, -0.187006591212369, "Pearson's r",
     "7. M AOA", "FALSE", "<unicode>", 0.0168185413866889, 3.96576280940113e+172,
     3760759763518041088, "<unicode>", "<unicode>", 1.31561371823867e+41,
     "BF<unicode><unicode>", "", "FALSE", "<unicode>", 0.0261972204151038,
     -0.343988442286973, -0.10342807197054, "", "", -0.161453361654432,
     "Upper 95% CI", "", "FALSE", "<unicode>", -0.0263947683888807,
     -0.389505876769038, -0.15514090775676, "", "", -0.212209360349994,
     "Lower 95% CI", "", 0, "TRUE", 0.224337251027458, 0.515089625720954,
     -0.40761023304153, 0.0782901793791844, 0.0377864041988949, -0.331147028692169,
     "<unicode>", 0.067885948274124, "Pearson's r", "8. M SIZE",
     "FALSE", 2.79727921467841e+60, "<unicode>", 1.45121575980472e+217,
     430595.16085099, 0.88603116068212, 1.5162988531292e+138, "<unicode>",
     6189.10421882693, "BF<unicode><unicode>", "", "FALSE", 0.249105256623709,
     "", -0.38534647844021, 0.104349739627225, 0.0640082860733409,
     -0.307443618800368, "<unicode>", 0.0939953686447663, "Upper 95% CI",
     "", "FALSE", 0.199155502914488, "", -0.429213770570984, 0.0520795224500734,
     0.0114912518726309, -0.354277798202797, "<unicode>", 0.041645310839735,
     "Lower 95% CI", "", "TRUE", 0.137705148213414, -0.127036888188237,
     0.126966582071929, -0.0615393412450079, -0.176597612716177,
     "<unicode>", 0.0556266752160016, 0.155271781270019, -0.386747184037372,
     "Pearson's r", "9. M GEND", "FALSE", 1.96932605727826e+21, 676604212019071360,
     643374631943187584, 627.617961443998, 2.52739165685628e+36,
     "<unicode>", 91.2152844438713, 4.28790580660044e+27, 2.80805860250888e+193,
     "BF<unicode><unicode>", "", "FALSE", 0.163372219432315, -0.101043153002401,
     0.152718139677499, -0.0352832276420161, -0.150954134214728,
     "<unicode>", 0.081787606845352, 0.180787794105185, -0.364058857534437,
     "Upper 95% CI", "", "FALSE", 0.111775737181379, -0.152787912061945,
     0.100972443083478, -0.0876764071997892, -0.201908842239297,
     "<unicode>", 0.029358059283273, 0.12946151162575, -0.408796656217612,
     "Lower 95% CI", ""))

  plotName <- results[["results"]][["matrixPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_bayesian-correlation-matrix-plot")

})

