context("Example: Glasgow Norms")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("CorrelationBayesian results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "Glasgow Norms.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("CorrelationBayesian", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["corBayesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", "Pearson's r", "<unicode>", "1. jaspColumn1", "FALSE",
     "BF<unicode><unicode>", "<unicode>", "", "FALSE", "Upper 95% CI",
     "<unicode>", "", "FALSE", "Lower 95% CI", "<unicode>", "", "TRUE",
     "Pearson's r", 0.317034386114287, "<unicode>", "2. jaspColumn2",
     "FALSE", "BF<unicode><unicode>", 7.31210394255761e+125, "<unicode>",
     "", "FALSE", "Upper 95% CI", 0.340414744670677, "<unicode>",
     "", "FALSE", "Lower 95% CI", 0.293100168727737, "<unicode>",
     "", 0, "TRUE", "Pearson's r", 0.347292142203392, 0.710030680865603,
     "<unicode>", "3. jaspColumn3", "FALSE", "BF<unicode><unicode>",
     1.24937976538968e+153, "<unicode>", "<unicode>", "", "FALSE",
     "Upper 95% CI", 0.370125010967267, "", "<unicode>", "", "FALSE",
     "Lower 95% CI", 0.323866089800627, "", "<unicode>", "", "TRUE",
     "Pearson's r", -0.264966534120283, 0.0563288554511697, 0.0391454758966565,
     "<unicode>", "4. jaspColumn4", "FALSE", "BF<unicode><unicode>",
     8.67823263444173e+85, 113.52463136621, 1.18473293817397, "<unicode>",
     "", "FALSE", "Upper 95% CI", -0.240274355237224, 0.0824870482477849,
     0.0653632996534428, "<unicode>", "", "FALSE", "Lower 95% CI",
     -0.289180249768599, 0.0300616276254229, 0.0128517542864677,
     "<unicode>", "", 0, "TRUE", "Pearson's r", -0.101350145944514,
     0.0909180351296638, 0.0694640934562379, 0.908251583996921, "<unicode>",
     "5. jaspColumn5", "FALSE", "BF<unicode><unicode>", 46436035825.3604,
     168810006.19827, 11325.2712056632, "<unicode>", "<unicode>",
     "", "FALSE", "Upper 95% CI", -0.0752263797864292, 0.116909511127017,
     0.0955663136095319, "", "<unicode>", "", "FALSE", "Lower 95% CI",
     -0.12727912439847, 0.0647514677525372, 0.0432276350173366, "",
     "<unicode>", "", "TRUE", "Pearson's r", 0.189292964841549, 0.245715550766478,
     0.191930721545424, 0.0924104875061614, 0.210418382849168, "<unicode>",
     "6. jaspColumn6", "FALSE", "BF<unicode><unicode>", 1.56267554264961e+42,
     1.83998188549447e+73, 2.82632940282763e+43, 362975534.588783,
     6.21099982811964e+52, "<unicode>", "", "FALSE", "Upper 95% CI",
     0.2144711636082, 0.2702024166554, 0.217080239916735, 0.118393368954689,
     0.235356852561227, "<unicode>", "", "FALSE", "Lower 95% CI",
     0.16376033654918, 0.220780301139872, 0.166422208855497, 0.0662496894946916,
     0.185089372785245, "<unicode>", "", 0, 0, "TRUE", "Pearson's r",
     -9.88699815533011e-05, -0.187006591212369, -0.129408034367433,
     -0.367055605434119, -0.476960453584576, -0.67226489778625, "<unicode>",
     "7. jaspColumn7", "FALSE", "BF<unicode><unicode>", 0.0168185413866889,
     1.31561371823867e+41, 3760759763518041088, 3.96576280940113e+172,
     "<unicode>", "<unicode>", "<unicode>", "", "FALSE", "Upper 95% CI",
     0.0261972204151038, -0.161453361654432, -0.10342807197054, -0.343988442286973,
     "", "", "<unicode>", "", "FALSE", "Lower 95% CI", -0.0263947683888807,
     -0.212209360349994, -0.15514090775676, -0.389505876769038, "",
     "", "<unicode>", "", 0, "TRUE", "Pearson's r", 0.515089625720954,
     0.067885948274124, 0.0782901793791844, -0.40761023304153, -0.331147028692169,
     0.0377864041988949, 0.224337251027458, "<unicode>", "8. jaspColumn8",
     "FALSE", "BF<unicode><unicode>", "<unicode>", 6189.10421882693,
     430595.16085099, 1.45121575980472e+217, 1.5162988531292e+138,
     0.88603116068212, 2.79727921467841e+60, "<unicode>", "", "FALSE",
     "Upper 95% CI", "", 0.0939953686447663, 0.104349739627225, -0.38534647844021,
     -0.307443618800368, 0.0640082860733409, 0.249105256623709, "<unicode>",
     "", "FALSE", "Lower 95% CI", "", 0.041645310839735, 0.0520795224500734,
     -0.429213770570984, -0.354277798202797, 0.0114912518726309,
     0.199155502914488, "<unicode>", "", "TRUE", "Pearson's r", -0.127036888188237,
     -0.386747184037372, -0.0615393412450079, 0.126966582071929,
     0.0556266752160016, -0.176597612716177, 0.137705148213414, 0.155271781270019,
     "<unicode>", "9. jaspColumn9", "FALSE", "BF<unicode><unicode>",
     676604212019071360, 2.80805860250888e+193, 627.617961443998,
     643374631943187584, 91.2152844438713, 2.52739165685628e+36,
     1.96932605727826e+21, 4.28790580660044e+27, "<unicode>", "",
     "FALSE", "Upper 95% CI", -0.101043153002401, -0.364058857534437,
     -0.0352832276420161, 0.152718139677499, 0.081787606845352, -0.150954134214728,
     0.163372219432315, 0.180787794105185, "<unicode>", "", "FALSE",
     "Lower 95% CI", -0.152787912061945, -0.408796656217612, -0.0876764071997892,
     0.100972443083478, 0.029358059283273, -0.201908842239297, 0.111775737181379,
     0.12946151162575, "<unicode>", ""))

  plotName <- results[["results"]][["matrixPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_bayesian-correlation-matrix-plot")

})

