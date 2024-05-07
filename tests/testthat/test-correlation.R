context("Correlation")

# test general results ----
options <- jaspTools::analysisOptions("Correlation")
options$vovkSellke <- TRUE
options$ci <- TRUE
options$significanceFlagged <- TRUE
options$kendallsTauB <- TRUE
options$assumptionCheckMultivariateShapiro <- TRUE
options$assumptionCheckPairwiseShapiro <- TRUE
options$scatterPlot <- TRUE
options$scatterPlotDensity <- TRUE
options$heatmapPlot <- TRUE
options$scatterPlotStatistic <- TRUE
options$sampleSize <- TRUE
options$spearman <- TRUE
options$effectSize <- TRUE
options$covariance <- TRUE
options$variables <- list("contNormal", "contGamma", "contcor1", "debMiss30")
set.seed(1)
results <- jaspTools::runAnalysis("Correlation", "debug.csv", options)


test_that("Shapiro-Wilk Test for Multivariate Normality table results match", {
  table <- results[["results"]][["assumptionsContainer"]][["collection"]][["assumptionsContainer_multivariateShapiro"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.951937236443559, 1.26292374637229e-05))
})

test_that("Shapiro-Wilk Test for Bivariate Normality table results match", {
  table <- results[["results"]][["assumptionsContainer"]][["collection"]][["assumptionsContainer_pairwiseShapiro"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.920133622718739, 9.26266088028011e-09, "-", "contNormal", "contGamma",
                                      0.975095766862836, 0.0197633225756002, "-", "contNormal", "contcor1",
                                      0.97421223116433, 0.108462090826961, "-", "contNormal", "debMiss30",
                                      0.930015026431291, 8.89751236523928e-08, "-", "contGamma", "contcor1",
                                      0.933648725575745, 2.37704417812631e-05, "-", "contGamma", "debMiss30",
                                      0.98166946114741, 0.416182497522526, "-", "contcor1", "debMiss30"
                                 ))
})

test_that("Correlation plot matches", {
  plotName <- results[["results"]][["corrPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "correlation-plot")
})

test_that("Kendall's tau B heatmap matches", {
  plotName <- results[["results"]][["heatmaps"]][["collection"]][["heatmaps_kendall"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "kendall-s-tau-b")
})

test_that("Pearson's r heatmap matches", {
  plotName <- results[["results"]][["heatmaps"]][["collection"]][["heatmaps_pearson"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "pearson-s-r")
})

test_that("Spearman's rho heatmap matches", {
  plotName <- results[["results"]][["heatmaps"]][["collection"]][["heatmaps_spearman"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "spearman-s-rho")
})

test_that("Correlation Table results match", {
  table <- results[["results"]][["mainTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.0960185736017089, -0.0266729903526464, -0.0266666666666667,
                                      -0.15041383947394, 0.694237192757787, 0.0677819401868667, 0.097080506140607,
                                      1, -0.0592696913271387, -0.0592003859505643, -0.252680329590477,
                                      0.558497687623534, 0.101534616513362, 0.138832075039338, 1,
                                      100, -0.0341927371158639, -0.0341794179417942, -0.229059752837501,
                                      0.73526094223706, 0.101287863086583, 0.163335243866025, 1, "<unicode>",
                                      "<unicode>", "<unicode>", "<unicode>", "<unicode>", "<unicode>",
                                      "<unicode>", "<unicode>", "<unicode>", "<unicode>", "<unicode>",
                                      "<unicode>", "<unicode>", "<unicode>", "<unicode>", "<unicode>",
                                      "<unicode>", "<unicode>", "<unicode>", "<unicode>", "<unicode>",
                                      "<unicode>", "<unicode>", 0.17245614334619, 0.0960518800942078,
                                      0.0957575757575758, -0.0373731390706183, 0.15805971278439, 0.0671525377474683,
                                      0.22888829058577, 1.26165085338952, 0.16244591532518, 0.161031927910319,
                                      -0.03654199812317, 0.109479317429059, 0.101534616513362, 0.346490687832583,
                                      1.51909334263147, 100, 0.143821784353644, 0.142838283828383,
                                      -0.0551264633902869, 0.156055917429528, 0.102156998059743, 0.329997969616898,
                                      1.26907384634445, -4.54234111641073, -0.142995727486937, -0.142028985507246,
                                      -0.302753498566225, 0.0820536231540238, 0.0798902375559328,
                                      0.0186955275517326, 1.7930869050848, -0.165268370300722, -0.163779936728643,
                                      -0.383976976749411, 0.175488795918533, 0.122169444356305, 0.0740435803355283,
                                      1.20465290217953, 70, -0.208206182304557, -0.20524888461202,
                                      -0.419968595404043, 0.0883143492445961, 0.1232177656224, 0.0312313683562874,
                                      1.71644871351761, "contNormal", "<unicode>", "<unicode>", "<unicode>",
                                      "<unicode>", "<unicode>", "<unicode>", "<unicode>", "<unicode>",
                                      "<unicode>", "<unicode>", "<unicode>", "<unicode>", "<unicode>",
                                      "<unicode>", "<unicode>", "<unicode>", "<unicode>", "<unicode>",
                                      "<unicode>", "<unicode>", "<unicode>", "<unicode>", "<unicode>",
                                      "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
                                      "", "", "", "", "", "", "", "", -0.242747263345942, -0.12919895977801,
                                      -0.128484848484848, -0.265695191109086, 0.0582140897855729,
                                      0.0666069384089473, 0.00872549413938947, 2.22231002833737, -0.157853482232319,
                                      -0.156555303374674, -0.342443190888167, 0.119832226549265, 0.101534616513362,
                                      0.0411274970992641, 1.44695679291394, 100, -0.185861370097632,
                                      -0.183750375037504, -0.3669254548718, 0.0673279518522942, 0.102488331218907,
                                      0.0131420647686214, 2.02506621791795, 6.82842148086829, 0.150610965569096,
                                      0.149482401656315, -0.0220394444690113, 0.0672280148907629,
                                      0.0796979487434949, 0.321004247781641, 2.02696064848969, 0.173519134850064,
                                      0.171798366528544, -0.065833220669967, 0.155001605969273, 0.122169444356305,
                                      0.39098888887008, 1.27306010334954, 70, 0.214387923136248, 0.211162627941562,
                                      -0.0250545433406204, 0.0793767652827101, 0.123275189177231,
                                      0.425046791840888, 1.82929064467251, "contGamma", "", "", "",
                                      "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
                                      "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
                                      "", "", "", "", "", "", "", "", "", "", "", "", "", "<unicode>",
                                      "<unicode>", "<unicode>", "<unicode>", "<unicode>", "<unicode>",
                                      "<unicode>", "<unicode>", "<unicode>", "<unicode>", "<unicode>",
                                      "<unicode>", "<unicode>", "<unicode>", "<unicode>", "<unicode>",
                                      "<unicode>", "<unicode>", "<unicode>", "<unicode>", "<unicode>",
                                      "<unicode>", "<unicode>", -2.14900575670369, -0.058451570593472,
                                      -0.0583850931677019, -0.226151894979294, 0.474719152682399,
                                      0.0813754587012183, 0.109381708643891, 1, -0.0906702415181398,
                                      -0.0904225863977578, -0.318626758463425, 0.456613508199801,
                                      0.122169444356305, 0.147689385556226, 1, 70, -0.102978167463976,
                                      -0.10261569416499, -0.329641395147143, 0.3970672317383, 0.122236141506121,
                                      0.135628607517475, 1, "contcor1", "", "", "", "", "", "", "",
                                      "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
                                      "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
                                      "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
                                      "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
                                      "", "", "<unicode>", "<unicode>", "<unicode>", "<unicode>",
                                      "<unicode>", "<unicode>", "<unicode>", "<unicode>", "<unicode>",
                                      "<unicode>", "<unicode>", "<unicode>", "<unicode>", "<unicode>",
                                      "<unicode>", "<unicode>", "<unicode>", "<unicode>", "<unicode>",
                                      "<unicode>", "<unicode>", "<unicode>", "<unicode>", "debMiss30"
                                 ))
})

test_that("Correlation Table hypothesis correlated positively match", {
  options <- jaspTools::analysisOptions("Correlation")
  options$ci <- TRUE
  options$pairwiseDisplay <- TRUE
  options$alternative <- "greater"
  options$kendallsTauB <- TRUE
  options$spearman <- TRUE
  options$variables <- list("contNormal", "contGamma", "contExpon")
  set.seed(1)
  results <- jaspTools::runAnalysis("Correlation", "debug.csv", options)
  table <- results[["results"]][["mainTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(-0.0266666666666667, -0.130518567835555, 0.652881403621106, 1,
                           -0.0592003859505642, -0.22249475793732, 0.720751156188233, 1,
                           "-", -0.0341794179417942, -0.198530526102106, 0.632414289086752,
                           1, "contNormal", "contGamma", 0.0824242424242424, -0.0362831074978719,
                           0.112169382715083, 1, 0.0628084903725342, -0.10374369323152,
                           0.267365049565566, 1, "-", 0.117203720372037, -0.0492249134641657,
                           0.122536070879884, 1, "contNormal", "contExpon", 0.0747474747474747,
                           -0.0315081778525471, 0.135251876148973, 1, 0.062688895089753,
                           -0.103862467527346, 0.267754890741034, 1, "-", 0.125616561656166,
                           -0.0407034694516623, 0.106321760175355, 1, "contGamma", "contExpon"
                      ))
})

test_that("Correlation Table hypothesis correlated negatively match", {
  options <- jaspTools::analysisOptions("Correlation")
  options$ci <- TRUE
  options$pairwiseDisplay <- TRUE
  options$alternative <- "less"
  options$kendallsTauB <- TRUE
  options$spearman <- TRUE
  options$variables <- list("contNormal", "contGamma", "contExpon")
  set.seed(1)
  results <- jaspTools::runAnalysis("Correlation", "debug.csv", options)
  table <- results[["results"]][["mainTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(-0.0266666666666667, -1, 0.347118596378894, 0.0771852345022219,
                           -0.0592003859505642, -1, 0.279248843811767, 0.107324940091343,
                           "-", -0.0341794179417942, -1, 0.36763047111853, 0.132041339755167,
                           "contNormal", "contGamma", 0.0824242424242424, -1, 0.887830617284917,
                           0.201131592346357, 0.0628084903725342, -1, 0.732634950434434,
                           0.225934274977044, "-", 0.117203720372037, -1, 0.877488217988468,
                           0.277299682807625, "contNormal", "contExpon", 0.0747474747474747,
                           -1, 0.864748123851027, 0.181003127347497, 0.062688895089753,
                           -1, 0.732245109258966, 0.225820332876708, "-", 0.125616561656166,
                           -1, 0.893700219352055, 0.285163043562837, "contGamma", "contExpon"
                      ))
})

# test error handling
test_that("Analysis handles errors", {
  options <- jaspTools::analysisOptions("Correlation")
  options$pairwiseDisplay <- TRUE
  options$variables <- list("contNormal", "debMiss99", "debSame")
  set.seed(1)
  results <- jaspTools::runAnalysis("Correlation", "debug.csv", options)
  table <- results[["results"]][["mainTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                      "NaN", "NaN", "-", "contNormal", "debMiss99", 1, 1, 1, 1, 1,
                                      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, "NaN", "NaN",
                                      "-", "contNormal", "debSame", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, "NaN", "NaN", "-", "debMiss99",
                                      "debSame"))
})


test_that("Pearson's partial correlation correct", {
  # Validated against Field, A. Discovering Statistics (5th edition). Chapter 8.5
  options <- jaspTools::analysisOptions("Correlation")
  options$pairwiseDisplay <- TRUE
  options$variables <- list("Exam", "Anxiety")
  options$partialOutVariables <- list("Revise")

  results <- jaspTools::runAnalysis("Correlation", "Exam Anxiety.csv", options)
  table <- results[["results"]][["mainTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.246665820246124, 0.0124458135120866, "-", "Exam", "Anxiety"
                                 ))
})

test_that("Concordance function works", {
  # old r functions
  concordanceFunction <- function(i, j) {
    concordanceIndicator <- 0
    ij <- (j[2] - i[2]) * (j[1] - i[1])
    if (ij > 0) concordanceIndicator <- 1
    if (ij < 0) concordanceIndicator <- -1
    return(concordanceIndicator)
  }
  addConcordances <- function(x, y, i) {
    concordanceIndex <- 0
    for (k in 1:length(x)) {
      if (k != i) {
        concordanceIndex <- concordanceIndex + concordanceFunction(c(x[i], y[i]), c(x[k], y[k]))
      }
    }
    return(concordanceIndex)
  }
  concordanceVector <- function(x, y){
    n <- length(x)
    concordanceSumsVector <- numeric(n)
    for (i in 1:n) {
      concordanceSumsVector[i] <- addConcordances(x, y, i)
    }
    return(concordanceSumsVector)
  }

  x <- rnorm(10)
  y <- rnorm(10)

  testthat::expect_equal(
    concordanceVector(x, y),
    jaspRegression:::concordanceVector_cpp(x, y))
})

test_that("Bootstrapping results match", {
  options <- jaspTools::analysisOptions("Correlation")
  options$pearson <- options$spearman <- options$kendallsTauB <- TRUE
  options$ci <- TRUE
  options$ciLevel <- 0.9
  options$ciBootstrap <- TRUE
  options$ciBootstrapSamples <- 100
  options$pairwiseDisplay <- TRUE
  options$variables <- list("contNormal", "contcor1", "debMiss30")
  options$partialOutVariables <- list("contcor2", "contGamma")

  set.seed(1)
  results <- jaspTools::runAnalysis("Correlation", "debug.csv", options)

  table <- results[["results"]][["mainTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.111065415602727, 0.00638598576879268, 0.105175421195814, 0.194277297531271,
                                      0.179666477474329, 0.0247584345587612, 0.0766902175159553, 0.328094117381122,
                                      "-", 0.195812907755063, -0.00131873961408421, 0.0533190778275557,
                                      0.344854893960198, "contNormal", "contcor1", -0.140516924209084,
                                      -0.310221936506635, 0.0901883731446404, -0.0198180653820397,
                                      -0.152460812916382, -0.389197019667127, 0.214536946611116, 0.0363882005780523,
                                      "-", -0.203777241507667, -0.447142385021777, 0.0955590913846829,
                                      -0.00255717986984448, "contNormal", "debMiss30", -0.0439397364731327,
                                      -0.152851036115505, 0.596221799762719, 0.0795466356087932, -0.103189761028142,
                                      -0.282205586036634, 0.402374946397542, 0.0781503832304625, "-",
                                      -0.0890439951881228, -0.260856052506065, 0.470233930910906,
                                      0.121736428310888, "contcor1", "debMiss30"))
})

test_that("Bootstrapping fails gracefully", {
  options <- jaspTools::analysisOptions("Correlation")
  options$variables <- c("contNormal", "debMiss99")
  options$partialOutVariables <- c("facFive")
  options$ciBootstrap <- TRUE
  options$ciBootstrapSamples <- 100
  options$ci <- TRUE

  set.seed(1)
  results <- jaspTools::runAnalysis("Correlation", "debug.csv", options)

  table <- results[["results"]][["mainTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                                      "<unicode>", "<unicode>", "<unicode>", "<unicode>", "NaN", "NaN",
                                      "NaN", "NaN", "contNormal", "", "", "", "", "<unicode>", "<unicode>",
                                      "<unicode>", "<unicode>", "debMiss99"))
})
