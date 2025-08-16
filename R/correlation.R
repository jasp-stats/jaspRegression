#
# Copyright (C) 2013-2020 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
CorrelationInternal <- function(jaspResults, dataset, options){

  ready <- length(options$variables) >= 2

  if (ready) {
    dataset <- .corrReadData(dataset, options)
    .hasErrors(dataset, type = c("infinity", "variance", "observations"),
               observations.amount = "< 3",
               observations.target = options$variables,
               exitAnalysisIfErrors = TRUE)
  }

  corrResults <- .corrMainResults(jaspResults, dataset, options, ready)

  .corrAssumptions(jaspResults, dataset, options, ready, corrResults)
  .corrPlot(jaspResults, dataset, options, ready, corrResults)
  .corrHeatmap(jaspResults, options, corrResults, ready)

  return()
}

.corrGetTests <- function(options){
  tests <- c("pearson", "spearman", "kendall")
  testsNames <- c(gettext("Pearson's"), gettext("Spearman's"), gettext("Kendall's Tau"))

  whichTests <- c(options[['pearson']], options[['spearman']], options[['kendallsTauB']])
  usedTests <- tests[whichTests]
  usedTestsNames <- testsNames[whichTests]

  return(list(
    tests = tests, testsNames = testsNames,
    usedTests = usedTests, usedTestsNames = usedTestsNames
  ))
}

.corrTestChecked <- function(test = c('pearson', 'spearman', 'kendallsTauB'), options){
  test <- match.arg(test) # ensures that 'kendall' is also valid

  return(isTRUE(options[[test]]))
}

# Preprocessing functions ----
.corrReadData <- function(dataset, options){
  if(length(options$partialOutVariables) == 0){
    cond <- FALSE
    vars <- options$variables
  } else{
    cond <- TRUE
    vars <- c(options$variables, options$partialOutVariables)
  }

  if(options$naAction == "pairwise"){
    data <- dataset[vars]
    if(cond) data <- excludeNaListwise(dataset, columns = options$partialOutVariables)
    return(data)
  } else if(options$naAction == "listwise"){
    return(excludeNaListwise(dataset, columns = vars))
  }
}

### Main function ----
.corrMainResults <- function(jaspResults, dataset, options, ready){
  if(!is.null(jaspResults[['mainTable']]) && !is.null(jaspResults[['results']])) return(jaspResults[['results']]$object)

  # Init main table
  mainTable <- .corrInitMainTable(jaspResults, options)

  # Compute results
  corrResults <- .corrComputeResults(jaspResults, dataset, options, ready)

  # Fill table
  .corrFillTableMain(mainTable, corrResults, options, ready)

  return(corrResults)
}
# Init tables ----
.corrInitMainTable <- function(jaspResults, options){
  if(!is.null(jaspResults[['mainTable']])) return(jaspResults[['mainTable']])

  if(length(options[['variables']]) == 0){
    variables <- c("...", "... ") # we need this trailing space so that 1 != 2
  } else if(length(options[['variables']]) == 1){
    variables <- c(options[['variables']], "...")
  } else {
    variables <- options[['variables']]
  }

  tests <- c(gettext("Pearson's"), gettext("Spearman's"), gettext("Kendall's Tau"))[c(options$pearson, options$spearman, options$kendallsTauB)]

  if(length(tests) != 1){
    if(length(options$partialOutVariables) == 0){
      title <- gettext("Correlation Table")
    } else{
      title <- gettext("Partial Correlation Table")
    }
  } else{
    if(length(options$partialOutVariables) == 0){
      title <- gettextf("%s Correlations", tests)
    } else{
      title <- gettextf("%s Partial Correlations", tests)
    }
  }

  mainTable <- createJaspTable(title = title)
  mainTable$dependOn(c("variables", "partialOutVariables",
                       "pearson", "spearman", "kendallsTauB", "pairwiseDisplay", "significanceReport",
                       "significanceFlagged", "sampleSize",
                       "ci", "ciLevel", "covariance",
                       "vovkSellke", "alternative", "naAction",
                       "ciBootstrap", "ciBootstrapSamples", "effectSize"))

  mainTable$position <- 1

  mainTable$showSpecifiedColumnsOnly <- TRUE

  if(options[['pairwiseDisplay']]){
    .corrInitPairwiseTable(mainTable, options, variables)
  } else{
    .corrInitCorrelationTable(mainTable, options, variables)
  }

  if(options[['alternative']] == "greater"){
    mainTable$addFootnote(message = gettext("All tests one-tailed, for positive correlation."))
    additionToFlagSignificant <- gettext(", one-tailed")
  } else if(options[['alternative']] == "less"){
    mainTable$addFootnote(message = gettext("All tests one-tailed, for negative correlation."))
    additionToFlagSignificant <- gettext(", one-tailed")
  } else{
    additionToFlagSignificant <- ""
  }
  if(options[['significanceFlagged']]) mainTable$addFootnote(message = gettextf("p < .05, ** p < .01, *** p < .001%s",
                                                                           additionToFlagSignificant), symbol = "*")

  if(length(options$partialOutVariables) > 0){
    message <- gettextf("Conditioned on variables: %s.", paste(options$partialOutVariables, collapse = ", "))
    mainTable$addFootnote(message = message)
  }

  if(length(options[["partialOutVariables"]]) != 0 && (isTRUE(options[["spearman"]]) || isTRUE(options[["kendallsTauB"]])))
    mainTable$addFootnote(message = gettext("Standard error of effect size (Fisher's z) is currently unavailable for non-parametric partial correlations."))

  if(length(options[["partialOutVariables"]]) != 0 && isTRUE(options[["ci"]]) && isFALSE(options[["ciBootstrap"]]))
    mainTable$addFootnote(message = gettext("Analytic confidence intervals for partial correlations are not yet available, but can be obtained using bootstrapping instead."))

  if(isTRUE(options[["ci"]] && isTRUE(options[["ciBootstrap"]])))
    mainTable$addFootnote(message = gettextf("Confidence intervals based on %i bootstrap replicates.", options[["ciBootstrapSamples"]]))

  # show
  jaspResults[['mainTable']] <- mainTable

  return(mainTable)
}


.corrInitPairwiseTable <- function(mainTable, options, variables){
  pairs <- combn(.v(variables), 2, simplify = FALSE)
  pairTitles <- combn(variables, 2, simplify = FALSE)

  mainTable$addColumnInfo(name = "variable1", title = "", type = "string")
  mainTable$addColumnInfo(name = "separator", title = "", type = "separator")
  mainTable$addColumnInfo(name = "variable2", title = "", type = "string")

  mainTable[['variable1']] <- sapply(pairTitles, function(x) x[[1]])
  mainTable[['separator']] <- rep("-", length(pairTitles))
  mainTable[['variable2']] <- sapply(pairTitles, function(x) x[[2]])


  mainTable$setExpectedSize(rows = length(pairs))
  for(row in 1:length(pairs)){
    rowName <- paste(pairs[[row]], collapse = "_")

    mainTable$setRowName(row, rowName)
  }

  options[["kendall"]] <- options[["kendallsTauB"]]

  tests <- c("pearson", "spearman", "kendall")
  nTests <- sum(unlist(options[tests]))
  testNames <- c(pearson=gettext("Pearson"), spearman=gettext("Spearman"), kendall=gettext("Kendall"))

  if(options$sampleSize) mainTable$addColumnInfo(name = "sample.size", title = "n", type = "integer")

  for(test in tests){
    if(options[[test]]){
      if(nTests != 1){
        overtitle <- testNames[test]
      } else{
        overtitle <- NULL
      }

      mainTable$addColumnInfo(  name = paste0(test, "_estimate"),       title = .corrTitlerer(test, nTests),                                        type = "number", overtitle = overtitle)

      if(options$significanceReport)
        mainTable$addColumnInfo(name = paste0(test, "_p.value"), title = gettext("p"), type = "pvalue", overtitle = overtitle)

      if(options$vovkSellke){
        mainTable$addColumnInfo(name = paste0(test, "_vsmpr"), title = gettext("VS-MPR"), type = "number", overtitle = overtitle)

        mainTable$addFootnote(message = .corrGetTexts()$footnotes$VSMPR, symbol = "\u2020", colNames = paste0(test, "_vsmpr"))
        mainTable$addCitation(.corrGetTexts()$references$Sellke_etal_2001)
      }

      if(options$ci){
        mainTable$addColumnInfo(name = paste0(test, "_lower.ci"),
                                title = gettextf("Lower %s%% CI", 100*options$ciLevel), type = "number",
                                overtitle = overtitle)
        mainTable$addColumnInfo(name = paste0(test, "_upper.ci"),
                                title = gettextf("Upper %s%% CI", 100*options$ciLevel), type = "number",
                                overtitle = overtitle)
      }

      if(options$effectSize){
        mainTable$addColumnInfo(name = paste0(test, "_effect.size"),    title = gettext("Effect size (Fisher's z)"),                                type = "number", overtitle = overtitle)
        mainTable$addColumnInfo(name = paste0(test, "_se.effect.size"), title = gettext("SE Effect size"),                                          type = "number", overtitle = overtitle)
      }
    }
  }

  if(options$covariance){
    mainTable$addColumnInfo(name = "covariance", title = gettext("Covariance"), type = "number")
  }
}

.corrTitlerer <- function(test, nTests){
  if(nTests > 1){
    coeffs <- c(pearson = gettext("r"), spearman = gettext("rho"), kendall = gettext("tau B"))
  } else{
    coeffs <- c(pearson = gettext("Pearson's r"), spearman = gettext("Spearman's rho"), kendall = gettext("Kendall's tau B"))
  }

  return(coeffs[test])
}

.corrInitCorrelationTable <- function(mainTable, options, variables){
  mainTable$transpose <- TRUE
  mainTable$transposeWithOvertitle <- FALSE

  mainTable$addColumnInfo(name = "var1", title = "", type = "string", combine = FALSE, overtitle = gettext("Variable"))

  whichtests <- c(options$pearson, options$spearman, options$kendallsTauB)

  #Apparently we are defining these titles over and over again, maybe .corrTitlerer could be reused or this stored somewhere globally?
  testsTitles <- c(gettext("Pearson's r"), gettext("Spearman's rho"), gettext("Kendall's Tau B"))[whichtests]
  tests <- c("pearson", "spearman", "kendall")[whichtests]

  for(vi in seq_along(variables)){
    overtitle <- paste(vi, variables[vi], sep = ". ")

    if(options$sampleSize) {
      mainTable$addColumnInfo(name = paste(variables[vi], "sample.size", sep = "_"), title = "n",
                              type = "integer", overtitle = overtitle)
    }

    for(ti in seq_along(tests)){
      .corrInitCorrelationTableRowAsColumn(mainTable, options, variables[vi], testsTitles[ti], tests[ti], overtitle)
    }

    if(options$covariance) {
      mainTable$addColumnInfo(name = paste(variables[vi], "covariance", sep = "_"), gettextf("Covariance"),
                              type = "number", overtitle = overtitle)
    }

    mainTable$setRowName(vi, .v(variables[vi]))
  }
}

.corrInitCorrelationTableRowAsColumn <- function(mainTable, options, var, coeff, test, overtitle){
  vvar <- .v(var)
  name <- paste(vvar, test, "%s", sep = "_")

  mainTable$addColumnInfo(name = sprintf(name, "estimate"),        title = coeff,                                                               type = "number", overtitle = overtitle)


  if(options$significanceReport)
    mainTable$addColumnInfo(name = sprintf(name, "p.value"), title = gettext("p-value"), type = "pvalue", overtitle = overtitle)

  if(options$vovkSellke){
    mainTable$addColumnInfo(name = sprintf(name, "vsmpr"), title = gettext("VS-MPR"), type = "number", overtitle = overtitle)

    mainTable$addFootnote(colNames = sprintf(name, "vsmpr"), symbol = "\u2020",
                          message = .corrGetTexts()$footnotes$VSMPR)
    mainTable$addCitation(.corrGetTexts()$references$Sellke_etal_2001)
  }


  if(options$ci){
    mainTable$addColumnInfo(name = sprintf(name, "lower.ci"),
                            title = gettextf("Lower %s%% CI", 100*options$ciLevel),
                            type = "number", overtitle = overtitle)
    mainTable$addColumnInfo(name = sprintf(name, "upper.ci"),
                            title = gettextf("Upper %s%% CI", 100*options$ciLevel),
                            type = "number", overtitle = overtitle)
  }

  if(options$effectSize){
    mainTable$addColumnInfo(name = sprintf(name, "effect.size"),    title = gettextf("Effect size (Fisher's z)"), type = "number", overtitle = overtitle)
    mainTable$addColumnInfo(name = sprintf(name, "se.effect.size"), title = gettext("SE Effect size"),            type = "number", overtitle = overtitle)
  }
}

### Compute results ----
.corrComputeResults <- function(jaspResults, dataset, options, ready){
  if(!ready) return()
  if(!is.null(jaspResults[['results']])) return(jaspResults[['results']]$object)

  vvars <- .v(options[['variables']])
  vcomb <- combn(vvars, 2, simplify = FALSE)
  vpair <- sapply(vcomb, paste, collapse = "_")

  pcor <- !length(options$partialOutVariables) == 0

  results <- list()
  #startProgressbar(length(vpair))
  for(i in seq_along(vpair)){
    # some variable pairs might be reusable, so we don't need to compute them again
    if(!is.null(jaspResults[[vpair[i]]])) {
      results[[vpair[i]]] <- jaspResults[[vpair[i]]]$object
    } else {
      data <- dataset[vcomb[[i]]]
      whichComplete <- complete.cases(data)
      data <- data[whichComplete,,drop=FALSE]

      if(pcor) {
        condData <- dataset[,.v(options$partialOutVariables), drop=FALSE]
        condData <- condData[whichComplete,,drop=FALSE]
      } else{
        condData <- NULL
      }

      errors <-.hasErrors(data, message = 'short',
                          type = c('variance', 'infinity', 'observations'),
                          all.target = .unv(vcomb[[i]]),
                          observations.amount = sprintf("< %s", 3+length(options$partialOutVariables)),
                          exitAnalysisIfErrors = FALSE)

      # for some reason .hasErrors does not flag this case
      if(nrow(data) == 0) errors <- list(observations = "All missing")

      # shorten the message for observations.amount (do not list variables which is apparent in the output)
      if(is.list(errors) && !is.null(errors$observations)){
        errors$message <- gettextf("Number of observations is < %s.", 3+length(options$partialOutVariables))
      }

      currentResults <- list()
      testErrors     <- list()
      currentResults[['sample.size']] <- nrow(data)
      if (isFALSE(errors))
        currentResults[['covariance']] <- cov(x = data[,1], y = data[,2])

      # even if we do not want the specific tests results
      # we still want the output as NaN - to fill the jaspTables correctly
      # so we still loop over all tests - .corr.test() returns empty lists if isFALSE(compute)
      for(test in c('pearson', 'spearman', 'kendall')){
        compute <- isFALSE(errors) && .corrTestChecked(test, options)

        r <- .corr.test(x = data[,1], y = data[,2], z = condData,
                        method = test, alternative = options[["alternative"]],
                        conf.interval = options$ci,
                        conf.level = options$ciLevel,
                        compute = compute, sample.size = currentResults[['sample.size']],
                        effect.size = options$effectSize,
                        se.effect.size = options$effectSize, options = options)

        testErrors[[test]] <- r[['errors']]
        currentResults[[test]] <- r[['result']]
      }
      # stolen from manova.R
      shapiro <- jaspAnova::.multivariateShapiroComputation(data, list(dependent = .unv(vcomb[[i]])))

      results[[vpair[i]]] <- list(vars       = .unv(vcomb[[i]]),
                                  vvars      = vcomb[[i]],
                                  res        = currentResults,
                                  errors     = errors,
                                  testErrors = testErrors,
                                  shapiro    = shapiro)

      # store state for pair
      state <- createJaspState(object = results[[vpair[i]]])
      state$dependOn(options = c("partialOutVariables", "alternative",
                                 "pearson", "spearman", "kendallsTauB",
                                 "ci", "ciLevel",
                                 "naAction"),
                     optionContainsValue = list(variables = .unv(vcomb[[i]])))

      jaspResults[[vpair[i]]] <- state
    }

    #progressbarTick()
  }

  if(options[["ciBootstrap"]])
    results <- .corrCalculateBootstrapCI(results, dataset, options)

  jaspResults[['results']] <- createJaspState(object = results)
  jaspResults[['results']]$dependOn(options = c("variables", "partialOutVariables",  "alternative",
                                                "ciLevel", "naAction",
                                                "pearson", "spearman", "kendallsTauB",
                                                "ciBootstrap", "ciBootstrapSamples"))


  return(results)
}

.corrCalculateBootstrapCI <- function(results, dataset, options) {
  alpha <- 1-options[["ciLevel"]]
  ciLevel <- c(alpha/2, 1-alpha/2)

  tests <- .corrGetTests(options)[["usedTests"]]

  # run bootstraps
  bootstraps <- array(NaN,
                      dim = c(options[["ciBootstrapSamples"]], length(results), length(tests)),
                      dimnames = list(NULL, names(results), tests))

  startProgressbar(expectedTicks = options[["ciBootstrapSamples"]], label = gettext("Bootstrapping"))
  for(i in seq_len(options[["ciBootstrapSamples"]])) {
    idx  <- sample(x = nrow(dataset), replace = TRUE)
    data <- dataset[idx,]

    if(length(options[["partialOutVariables"]]) > 0) {
      z <- data[,options[["partialOutVariables"]], drop=FALSE]
    } else {
      z <- NULL
    }

    for(name in names(results)) {
      xy <- data[, results[[name]][["vars"]]]

      for(test in tests){
        bootstraps[i, name, test] <- tryCatch(
          .corrFastParCor(xy = xy, z = z, method = test),
          error = function(e) NaN
          )
      }
    }

    progressbarTick()
  }

  # replace CIs in the results list with the bootstrapped ones
  for(name in names(results)) {
    for(test in tests){
      ci <- quantile(bootstraps[,name,test], probs = ciLevel, na.rm = TRUE)
      ci[is.na(ci)] <- NaN
      results[[name]][["res"]][[test]][c("lower.ci", "upper.ci")] <- ci
    }
  }

  return(results)
}

.corrFastParCor <- function(xy, z = NULL, method = c("pearson", "spearman", "kendall")) {
  if(is.null(z)) return(stats::cor(xy[,1], xy[,2], use = "p", method = method))

  xyz <- cbind(xy, z)
  xyz <- na.omit(xyz)

  cmat  <- stats::cov(xyz, method = method)
  icmat <- chol2inv(chol(cmat))
  pcmat <- -stats::cov2cor(icmat)

  return(pcmat[1,2,drop=TRUE])
}

# helper that unifies output of cor.test and ppcor::pcor.test

.corr.test <- function(x, y, z = NULL, alternative = c("twoSided", "greater", "less"), method, exact = NULL, conf.interval = TRUE, conf.level = 0.95, continuity = FALSE, compute=TRUE, sample.size, options, ...){
  stats <- c("estimate", "p.value", "conf.int", "vsmpr",  "effect.size", "se.effect.size")
  statsNames <- c("estimate", "p.value", "lower.ci", "upper.ci", "vsmpr", "effect.size", "se.effect.size")
  alternative <- match.arg(alternative)

  if(isFALSE(compute)){
    result <- rep(NaN, length(statsNames))
    names(result) <- statsNames
    errors <- FALSE
  } else if(is.null(z)){
    # base methods do not follow our style :(
    alt <- if(alternative == "twoSided") "two.sided" else alternative
    result <- try(expr = {
      cor.test(x = x, y = y, alternative = alt, method = method, exact = exact,
               conf.level = conf.level, continuity = continuity, ... = ...)}, silent = TRUE)

    if(isTryError(result)) {
      errors <- .extractErrorMessage(result)
      result <- rep(NaN, length(statsNames))
      names(result) <- statsNames
    } else{
      errors <- FALSE

      if(method != "pearson" && conf.interval){
        result$conf.int <- .createNonparametricConfidenceIntervals(x = x, y = y, obsCor = result$estimate,
                                                                   alternative = alternative, confLevel = conf.level,
                                                                   method = method)
      } else if(is.null(result$conf.int)){
        result$conf.int <- c(NA, NA)
      }

      result$vsmpr <- jaspBase:::VovkSellkeMPR(result$p.value)
      result$vsmpr <- ifelse(result$vsmpr == "∞", Inf, result$vsmpr)

      #effect size (fisher's z) and SE for Spearman and Kendall are computed following the recommendations of
      #Caruso, J.C., & Cliff, N. (1997). Empirical Size, Coverage, and Power of Confidence Intervals for Spearman's Rho. Educational and Psychological Measurement, 57(4), 637-654.
      #Xu, W., Hou, Y., Hung, Y.S., & Zou, Y. (2013). A comparative analysis of Spearman’s rho and Kendall’s tau in normal and contaminated normal models. Signal Processing, 93, 261-276.
      result$effect.size <- atanh(result$estimate)
      if(method == "pearson")
        result$se.effect.size <- sqrt(1/(sample.size-3))
      if(method == "spearman") {
        n <- sample.size
        result$se.effect.size <- sqrt((1 / (n-2)) + (abs(atanh(result$estimate)) / ((6 * n) + (4 * n)^(1/2))))
      }
      if(method == "kendall") {
        n <- sample.size
        s1 <- asin(sin((pi/2) * result$estimate))
        s2 <- asin(sin((pi/2) * result$estimate)/2)
        result$se.effect.size <- sqrt((2 / (n * (n - 1))) * (1 - (4 * (s1^2 / pi^2)) + (2 * (n - 2) * ((1/9) - (4 * (s2^2 / pi^2))))))
      }

      result <- unlist(result[stats], use.names = FALSE)
      names(result) <- statsNames
    }
  } else{
    result <- try(expr = {ppcor::pcor.test(x = x, y = y, z = z, method = method)}, silent = TRUE)
    if(isTryError(result)) {
      errors <- .extractErrorMessage(result)
      if(startsWith(errors, "reciprocal condition number")) errors <- gettext("Partial correlation cannot be computed: covariance matrix is computationally singular.")
      result <- rep(NaN, length(statsNames))
      names(result) <- statsNames
      result$lower.ci <- NA
      result$upper.ci <- NA
    } else{
      errors <- FALSE
      result <- as.list(result)
      if(alternative == "less"){
        if(result$estimate <= 0){
          result$p.value <- result$p.value/2
        } else{
          result$p.value <- 1 - result$p.value/2
        }
      } else if(alternative == "greater"){
        if(result$estimate >= 0){
          result$p.value <- result$p.value/2
        } else{
          result$p.value <- 1 - result$p.value/2
        }
      }
      result$vsmpr <- jaspBase:::VovkSellkeMPR(result$p.value)
      result$vsmpr <- ifelse(result$vsmpr == "∞", Inf, result$vsmpr)
      # TODO: CIs for partial correlations
      result$lower.ci <- NA
      result$upper.ci <- NA

      #effect size (fisher's z) and SE are computed following the recommendations of
      #Fieller, E. C., Hartley, H. O., & Pearson, E. S. (1957). Tests for rank correlation coefficients: I. Biometrika, 44, 470–481.
      result$effect.size <- atanh(result$estimate)
      if(method == "pearson")
        result$se.effect.size <- sqrt(1/(sample.size-3-length(options$partialOutVariables)))
      if(method == "spearman")
        result$se.effect.size <- NA
      if(method == "kendall")
        result$se.effect.size <- NA

      result <- unlist(result[statsNames], use.names = FALSE)
      names(result) <- statsNames
    }
  }

  return(list(result = result, errors = errors))
}


.corrAssumptions <- function(jaspResults, dataset, options, ready, corrResults){
  # uses .multivariateShapiroComputation from manova.R
  if(isFALSE(options$assumptionCheckMultivariateShapiro) && isFALSE(options$assumptionCheckPairwiseShapiro)) return()

  if(is.null(jaspResults[['assumptionsContainer']])){
    assumptionsContainer <- createJaspContainer(title = gettext("Assumption checks"))
    assumptionsContainer$dependOn(c("variables", "partialOutVariables"))
    assumptionsContainer$position <- 2

    jaspResults[['assumptionsContainer']] <- assumptionsContainer
  } else {
    assumptionsContainer <- jaspResults[['assumptionsContainer']]
  }

  if(isTRUE(options$assumptionCheckMultivariateShapiro) && is.null(assumptionsContainer[['assumptionCheckMultivariateShapiro']]))
    .corrMultivariateShapiro(assumptionsContainer, dataset, options, ready, corrResults)

  if(isTRUE(options$assumptionCheckPairwiseShapiro) && is.null(assumptionsContainer[['pairwiseShapiro']]))
    .corrPairwiseShapiro(assumptionsContainer, dataset, options, ready, corrResults)

}

.corrMultivariateShapiro <- function(assumptionsContainer, dataset, options, ready, corrResults){
  shapiroTable <- createJaspTable(title = gettext("Shapiro-Wilk Test for Multivariate Normality"))
  shapiroTable$dependOn("assumptionCheckMultivariateShapiro")
  shapiroTable$position <- 1
  shapiroTable$showSpecifiedColumnsOnly <- TRUE

  if(length(options$partialOutVariables) == 0){

    shapiroTable$addColumnInfo(name = "W", title = gettext("Shapiro-Wilk"), type = "number")
    shapiroTable$addColumnInfo(name = "p", title = gettext("p"), type = "pvalue")

    assumptionsContainer[['multivariateShapiro']] <- shapiroTable

    if(ready) {
      dataset <- dataset[complete.cases(dataset),,drop=FALSE]
      shapiroResult <- jaspAnova::.multivariateShapiroComputation(dataset, list(dependent = options$variables))
      shapiroErrors <- shapiroResult$errors
      shapiroResult <- shapiroResult$result
      shapiroTable$addRows(list(W = shapiroResult$statistic, p = shapiroResult$p.value))

      if (!is.null(shapiroErrors))shapiroTable$setError(shapiroErrors)
    }
  } else{
    shapiroTable$addColumnInfo(name = "vars", title = gettext("Variables"),    type = "string")
    shapiroTable$addColumnInfo(name = "W",    title = gettext("Shapiro-Wilk"), type = "number")
    shapiroTable$addColumnInfo(name = "p",    title = gettext("p"),            type = "pvalue")

    assumptionsContainer[['multivariateShapiro']] <- shapiroTable

    if(ready){
      dataset <- dataset[complete.cases(dataset),,drop=FALSE]
      shapiroResult <- list()
      shapiroResult[['All']]          <- jaspAnova::.multivariateShapiroComputation(dataset, list(dependent = c(options$variables,
                                                                                                     options$partialOutVariables)))
      shapiroResult[['Conditioned']]  <- jaspAnova::.multivariateShapiroComputation(dataset, list(dependent = options$variables))
      shapiroResult[['Conditioning']] <- jaspAnova::.multivariateShapiroComputation(dataset, list(dependent = options$partialOutVariables))

      form <- sprintf("cbind(%s) ~ %s",
                      paste(.v(options$variables), collapse = ", "),
                      paste(.v(options$partialOutVariables), collapse = " + "))
      resids <- try(expr = {residuals(lm(formula = form, data = dataset))}, silent = TRUE)

      if(isTryError(resids)){
        shapiroResult[['Residuals']] <- list(errors = .extractErrorMessage(resids))
      } else{
        shapiroResult[['Residuals']] <- jaspAnova::.multivariateShapiroComputation(resids, list(dependent = options$variables))
      }

      for(i in seq_along(shapiroResult)){
        if(!is.null(shapiroResult[[i]]$errors)){
          shapiroTable$setError(shapiroResult[[i]]$errors)
          break
        }
        shapiroTable$addRows(list(vars = names(shapiroResult)[i],
                                  W    = shapiroResult[[i]]$result$statistic,
                                  p    = shapiroResult[[i]]$result$p.value))
      }
    }
  }
}

.corrPairwiseShapiro <- function(assumptionsContainer, dataset, options, ready, corrResults){
  shapiroTable <- createJaspTable(title = gettext("Shapiro-Wilk Test for Bivariate Normality"))
  shapiroTable$dependOn(c("assumptionCheckPairwiseShapiro", "naAction"))
  shapiroTable$position <- 2
  shapiroTable$showSpecifiedColumnsOnly <- TRUE

  shapiroTable$addColumnInfo(name = "var1",      title = "",                      type = "string")
  shapiroTable$addColumnInfo(name = "separator", title = "",                      type = "separator")
  shapiroTable$addColumnInfo(name = "var2",      title = "",                      type = "string")
  shapiroTable$addColumnInfo(name = "W",         title = gettext("Shapiro-Wilk"), type = "number")
  shapiroTable$addColumnInfo(name = "p",         title = gettext("p"),            type = "pvalue")

  shapiroTable$setExpectedSize(rows = max(1, choose(length(options$variables), 2)))

  assumptionsContainer[['pairwiseShapiro']] <- shapiroTable

  if(ready){
    for(i in seq_along(corrResults)){
      res <- corrResults[[i]]

      shapiroTable$addRows(list(
        var1 = res$vars[1], separator = "-", var2 = res$vars[2],
        W = res$shapiro$result$statistic, p = res$shapiro$result$p.value
      ))


      name <- paste(res$vvars, collapse = "_")
      shapiroTable$setRowName(rowIndex = i, newName = name)

      if(!is.null(res$shapiro$errors))  shapiroTable$addFootnote(message = res$shapiro$errors, rowNames = name)
    }
  }
}

### Fill Tables ----
.corrFillTableMain <- function(mainTable, corrResults, options, ready){
  if(!ready) return()

  if(options$pairwiseDisplay){
    .corrFillPairwiseTable(mainTable, corrResults, options)
  } else{
    .corrFillCorrelationMatrix(mainTable, corrResults, options)
  }
}

.corrFillPairwiseTable <- function(mainTable, corrResults, options){
  # extract the list of results
  pairs <- names(corrResults)
  results <- lapply(corrResults, function(x) x[['res']])
  errors <- lapply(corrResults,function(x) x[['errors']])
  testErrors     <- lapply(corrResults, function(x) x[['testErrors']])

  mainTable[['sample.size']] <- sapply(results, function(x) x[['sample.size']])
  mainTable[['covariance']] <- sapply(results, function(x) x[['covariance']])

  # would be nice to be able to fill table cell-wise, i.e., mainTable[[row, col]] <- value
  colNames <- character() # this is for error footnotes
  for(test in c("pearson", "spearman", "kendall")){
    res <- data.frame(do.call(rbind, lapply(results, function(x) {x[[test]]})), stringsAsFactors = FALSE)

    for(col in colnames(res)) mainTable[[paste(test, col, sep = "_")]] <- res[[col]]

    if(options[['significanceFlagged']]) .corrFlagSignificant(mainTable, res[['p.value']],
                                                          paste(test, "estimate", sep="_"), pairs)

    colNames <- c(colNames, paste(test, colnames(res), sep="_"))
  }
  for(i in seq_along(errors)){
    # add footnotes for general errors
    if(!isFALSE(errors[[i]])) mainTable$addFootnote(message = errors[[i]]$message, rowNames = pairs[i],
                                                    colNames = colNames)

    # add footnotes for test specific errors
    for(test in c("pearson", "spearman", "kendall")){
      if(!isFALSE(testErrors[[i]][[test]])){
        errorColNames <- colNames[startsWith(colNames, test)]
        mainTable$addFootnote(message = testErrors[[i]][[test]], rowNames = pairs[i], colNames = errorColNames)
      }
    }
  }


}

.corrFlagSignificant <- function(table, p.values, colName, rowNames){
  p.values <- as.numeric(p.values)

  s <- rowNames[!is.na(p.values) & !is.nan(p.values) & p.values < 0.05 & p.values >= 0.01]
  if(length(s) > 0){
    table$addFootnote(colNames = colName, rowNames = s, symbol = "*")
  }

  ss <- rowNames[!is.na(p.values) & !is.nan(p.values) & p.values < 0.01 & p.values >= 0.001]
  if(length(ss) > 0){
    table$addFootnote(colNames = colName, rowNames = ss, symbol = "**")
  }

  sss <- rowNames[!is.na(p.values) & !is.nan(p.values) & p.values < 0.001]
  if(length(sss) > 0){
    table$addFootnote(colNames = colName, rowNames = sss, symbol = "***")
  }
}

.corrFillCorrelationMatrix <- function(mainTable, corrResults, options){
  vars  <- options$variables
  vvars <- .v(vars)
  pairs <- strsplit(names(corrResults), "_")

  results <- lapply(corrResults, function(x) {
    res <- unlist(x[['res']]) # flatten the structure but preserve names (hierarchy separated by ".")
    # replace the first dot with _ to separate test from statistic
    names(res) <- sub("(pearson\\.)",  "pearson_", names(res))
    names(res) <- sub("(spearman\\.)", "spearman_", names(res))
    names(res) <- sub("(kendall\\.)",  "kendall_", names(res))
    res
    })

  errors         <- lapply(corrResults, function(x) x[['errors']])
  testErrors     <- lapply(corrResults, function(x) x[['testErrors']])
  statsNames     <- names(results[[1]])
  nStats         <- length(statsNames)

  # would be really (!) nice to be able to fill table cell-wise, i.e., mainTable[[row, col]] <- value
  # in the meantime we have to collect and fill the entire table in the resultList
  resultList <- list(var1 = vars)
  for(colVar in seq_along(vars)){
    for(statName in statsNames){
      currentColumnName <- paste(vvars[[colVar]], statName, sep = "_")
      resList <- list()
      for(rowVar in seq_along(vars)){
        currentPairName <- paste(vvars[rowVar], vvars[colVar], sep = "_")
        if(rowVar == colVar){
          r <- "\u2014" # long dash
        } else if(rowVar > colVar){
          r <- NA # upper triangle is empty
        } else {
          r <- results[[currentPairName]][[statName]]
        }
        resList[[vvars[rowVar]]] <- r
      }
      resultList[[currentColumnName]] <- resList
    }
  }

  mainTable$setData(resultList)

  # Flag significant
  if(options[['significanceFlagged']]){
    p.valueColumns <- names(resultList)[endsWith(names(resultList), "p.value")]

    for(columnName in p.valueColumns){
      p.values <- unlist(resultList[[columnName]])
      .corrFlagSignificant(table = mainTable, p.values = p.values,
                           colName = gsub("p.value", "estimate", columnName),
                           rowNames = vvars)
    }
  }

  # Report errors as footnotes
  for(i in seq_along(errors)){
    # display general errors (i.e., too much missing data, etc. identified from .hasErrors)
    if(is.list(errors[[i]])){
      pair <- pairs[[i]]
      colNames <- statsNames[statsNames != "sample.size"]
      colNames <- paste(pair[2], colNames, sep = "_")
      mainTable$addFootnote(message = errors[[i]]$message, colNames = colNames, rowNames = pair[1])
    }

    # display test errors (i.e., during calculating results, such as failure to invert a correlation matrix, etc.)
    for (test in c("pearson", "spearman", "kendall")) {
      if (!isFALSE(testErrors[[i]][[test]])) {
        pair <- pairs[[i]]
        colNames <- statsNames[startsWith(statsNames, test)]
        colNames <- paste(pair[2], colNames, sep = "_")
        mainTable$addFootnote(message = testErrors[[i]][[test]], colNames = colNames, rowNames = pair[1])
      }
    }
  }
}
### Plots ----

# Helper function that, if partial correlation takes place, transforms X and Y variables into residuals X and Y regressed on Z.
# If no variables Z to partial out are specified, it simply extracts normal X and Y variables from dataset according to col_id.
# Presence of Z needs to be determine by a boolean "pcor" beforehand.
.corrExtractVarOrRes <- function(pcor, col_id, dataset, options){
  data <- dataset[col_id]
  data <- data[complete.cases(data), ]
  if (!pcor) return (data)

  condData <- dataset[,.v(options$partialOutVariables), drop = FALSE]
  condData <- condData[complete.cases(data),, drop = TRUE]

  # For multiple variables Z, create intermediate data frames to allow regressing either X OR Y on all Z variables
  if (is.data.frame(condData)) {
    intermediateXOnZ <- data.frame(X = data[, 1, drop=TRUE], condData)
    intermediateYOnZ <- data.frame(Y = data[, 2, drop=TRUE], condData)
    regressXOnZ <- lm(X ~ ., data=intermediateXOnZ)
    regressYOnZ <- lm(Y ~ ., data=intermediateYOnZ)
  }
  else {
    regressXOnZ <- lm(data[,1, drop=TRUE] ~ condData)
    regressYOnZ <- lm(data[,2,drop=TRUE] ~ condData)
  }

  data[,1] <- regressXOnZ$residuals
  data[,2] <- regressYOnZ$residuals

  return(data)
}

# Helper function to add captions under plots, borrowed from rainbow plots in jaspDescriptives.
# Unfortunately it cannot put captions under jasp::ggMatrixPlot objects (yet).
.corrPlotCaption <- function(plotObject, caption){
  addCaption      <- ggplot2::labs(caption = caption)
  captionPosition <- ggplot2::theme(plot.caption = ggtext::element_markdown(hjust = 0))  # Bottom left position

  return(plotObject + addCaption + captionPosition)
}

# Wrapper for reDrawJaspGraphsPlot to include optional caption under grob compsite. To use:
# - Needs to replace member function in plot$plotObject$plotFunction when needed.
# - Caption argument needs to be created in member list plot$plotObject$plotArgs$caption.
# NOTE: Currently UNSTABLE since it's using jaspGraphs internals without exporting.
.corrRedrawWithCaption <- function(subplots, args, grob = FALSE, newpage = TRUE,
                                   decodeplotFun = jaspGraphs:::getDecodeplotFun(), ...) {

  # The original grob to wrap around, without rendering
  grob_mat <- jaspGraphs:::reDrawJaspGraphsPlot(subplots, args, grob = TRUE, newpage = FALSE,
                                                decodeplotFun = decodeplotFun, ...)

  # New composite grob, with optional caption
  caption <- args[["caption"]]
  if (!is.null(caption)) {
    bottom_grob <- grid::textGrob(caption)
    modJaspGraphsPlot <- gridExtra::arrangeGrob(grob_mat, bottom = bottom_grob)
    print(get0("reDrawJaspGraphsPlot", envir = asNamespace("jaspGraphs")))
  }
  else{
    modJaspGraphsPlot <- grob_mat
  }

  # Return grob and don't render, if desired
  if (grob){
    return(modJaspGraphsPlot)
  }

  # Render grob
  gridExtra::grid.arrange(modJaspGraphsPlot, newpage = newpage)
}

.corrPlot <- function(jaspResults, dataset, options, ready, corrResults){
  if (!ready) return()
  if (isFALSE(options$scatterPlot)) return()

  wantsOnlyScatter <-  isFALSE(options[["scatterPlotDensity"]] || options[["scatterPlotStatistic"]])
  if (isTRUE(options[["pairwiseDisplay"]]) || (wantsOnlyScatter && length(options[["variables"]]) == 2)) {
    .corrPairwisePlot(jaspResults, dataset, options, ready, corrResults)
  } else {
    .corrMatrixPlot(jaspResults, dataset, options, ready, corrResults)
  }
}

.corrPairwisePlot <- function(jaspResults, dataset, options, ready, corrResults, errors=NULL){
  if(!is.null(jaspResults[['corrPlot']])) return()

  plotContainer <- createJaspContainer(title = gettext("Scatter plots"))
  plotContainer$dependOn(options = c("variables", "partialOutVariables", "pearson", "spearman", "kendallsTauB",
                                     "pairwiseDisplay", "ci", "ciLevel", "alternative", "ciBootstrap", "ciBootstrapSamples",
                                     "scatterPlot", "scatterPlotDensity", "scatterPlotStatistic", "scatterPlotCi",
                                     "scatterPlotCiLevel", "scatterPlotPredictionIntervalLevel",
                                     "scatterPlotPredictionInterval", "naAction"))
  plotContainer$position <- 3
  jaspResults[['corrPlot']] <- plotContainer

  vars <- options$variables
  vvars <- .v(vars)

  comb <- combn(vars, 2, simplify = FALSE)
  pairs <- sapply(comb, paste, collapse = " vs. ")
  vcomb <- combn(vvars, 2, simplify = FALSE)
  vpairs <- sapply(vcomb, paste, collapse = "_")

  pcor <- length(options$partialOutVariables) != 0
  pcorCaption <- sprintf("Axes show residuals conditioned on variables: %s ",
                         paste0(decodeColNames(options$partialOutVariables), collapse = ", "))

  if(options[['scatterPlotDensity']]){
    for(i in seq_along(vcomb)){
      plot <- createJaspPlot(title = pairs[i], width = 550, height = 550)
      plotContainer[[vpairs[i]]] <- plot

      plotMat <- matrix(list(), 2, 2)

      data <- .corrExtractVarOrRes(pcor, vcomb[[i]], dataset, options)

      plotMat[[1, 1]] <- .corrMarginalDistribution(variable = data[,1,drop=TRUE], varName = comb[[i]][1],
                                                   options = options, yName = NULL)
      plotMat[[2, 2]] <- .corrMarginalDistribution(variable = data[,2,drop=TRUE], varName = comb[[i]][2],
                                                   options = options, yName = NULL, coord_flip = TRUE)
      plotMat[[1, 2]] <- .corrValuePlot(corrResults[[vpairs[i]]], options = options)

      # get consistent breaks for scatterplot with the densities
      var1Breaks <- try(expr = {ggplot2::ggplot_build(plotMat[[1, 1]])$layout$panel_params[[1]]$x.major_source},
                        silent=TRUE)
      if(isTryError(var1Breaks)) var1Breaks <- NULL
      var2Breaks <- try(expr = {ggplot2::ggplot_build(plotMat[[2, 2]])$layout$panel_params[[1]]$y.major_source},
                        silent=TRUE)
      if(isTryError(var2Breaks)) var2Breaks <- NULL
      plotMat[[2, 1]] <- .corrScatter(xVar = data[,1,drop=TRUE], yVar = data[,2,drop=TRUE],
                                      options = options,
                                      xBreaks = var1Breaks,
                                      yBreaks = var2Breaks,
                                      drawAxes = FALSE)

      plot$plotObject <- jaspGraphs::ggMatrixPlot(plotMat,
                                                bottomLabels = c(comb[[i]][1],       gettext("Density")),
                                                leftLabels   = c(gettext("Density"), comb[[i]][2]))
      if (pcor){
        plot$plotObject$plotFunction <- .corrRedrawWithCaption
        plot$plotObject$plotArgs$caption <- pcorCaption
      }
    }
  } else if(options[['scatterPlotStatistic']]){
    for(i in seq_along(vcomb)){
      plot <- createJaspPlot(title = pairs[i], width = 600, height = 300)
      plotContainer[[vpairs[i]]] <- plot

      data <- .corrExtractVarOrRes(pcor, vcomb[[i]], dataset, options)

      plotMat <- matrix(list(), 1, 2)
      plotMat[[1, 1]] <- .corrScatter(xVar = data[,1,drop=TRUE], yVar = data[,2,drop=TRUE],
                                      options = options,
                                      xName = comb[[i]][1], yName = comb[[i]][2],
                                      drawAxes = TRUE)

      plotMat[[1, 2]] <- .corrValuePlot(corrResults[[vpairs[i]]], options = options)

      plot$plotObject <- jaspGraphs::ggMatrixPlot(plotMat)

      if (pcor){
        plot$plotObject$plotFunction <- .corrRedrawWithCaption
        plot$plotObject$plotArgs$caption <- pcorCaption
      }
    }
  } else{
    for(i in seq_along(vcomb)){
      plot <- createJaspPlot(title = pairs[i], width = 400, height = 400)
      plotContainer[[vpairs[i]]] <- plot

      data <- .corrExtractVarOrRes(pcor, vcomb[[i]], dataset, options)
      plot$plotObject <- .corrScatter(xVar = data[,1,drop=TRUE], yVar = data[,2,drop=TRUE],
                        options = options,
                        xName = comb[[i]][1], yName = comb[[i]][2],
                        drawAxes = TRUE)
      if (pcor){
        plot$plotObject <- .corrPlotCaption(plot$plotObject, pcorCaption)
      }
    }
  }
}

.corrMatrixPlot <- function(jaspResults, dataset, options, ready, corrResults, errors=NULL){
  if(!is.null(jaspResults[['corrPlot']])) return()
  vars <- options$variables
  vvars <- .v(vars)
  len <- length(vars)
  pcor <- (length(options$partialOutVariables) != 0)
  pcorCaption <- sprintf("Axes show residuals conditioned on variables: %s ",
                         paste0(decodeColNames(options$partialOutVariables), collapse = ", "))

  plot <- createJaspPlot(title = gettext("Correlation plot"))
  plot$dependOn(options = c("variables", "partialOutVariables", "pearson", "spearman", "kendallsTauB",
                            "pairwiseDisplay", "ci", "ciLevel", "alternative",
                            "scatterPlot", "scatterPlotDensity", "scatterPlotStatistic", "scatterPlotCi",
                            "scatterPlotCiLevel", "scatterPlotPredictionIntervalLevel",
                            "scatterPlotPredictionInterval", "naAction"))
  plot$position <- 3

  if (len <= 2 && (options$scatterPlotDensity || options$scatterPlotStatistic)) {
    plot$width <- 580
    plot$height <- 580
  } else if (len <= 2) {
    plot$width <- 400
    plot$height <- 400
  } else {
    plot$width <- 250 * len + 20
    plot$height <- 250 * len + 20
  }


  jaspResults[['corrPlot']] <- plot


  plotMat <- matrix(list(), len, len)
  for(row in seq_len(len)){
    for(col in seq_len(len)){

      data <- .corrPartialResiduals(pcor, vvars[c(col, row)], dataset, options)
      #data <- dataset[,vvars[c(col,row)],drop=FALSE]
      #data <- data[complete.cases(data),,drop=FALSE]

      if(row == col) {
        plotMat[[row, col]] <- .corrMarginalDistribution(variable = data[,1,drop=TRUE],
                                                         varName = vars[col],
                                                         options = options, errors = errors)
      } else if(row > col){
        plotMat[[row, col]] <- .corrValuePlot(corrResults[[paste(vvars[c(col, row)], collapse = "_")]],
                                              options = options)
      } else {
        plotMat[[row, col]] <- .corrScatter(xVar = data[,1,drop=TRUE], yVar = data[,2,drop=TRUE],
                                            options = options)
        if(pcor){
          plotMat[[row, col]] <- .corrPlotCaption(plotMat[[row, col]], pcorCaption)
        }
      }
    }
  }

  p <- jaspGraphs::ggMatrixPlot(plotList = plotMat, leftLabels = vars, topLabels = vars,
                                scaleXYlabels = NULL)
  plot$plotObject <- p

  if (pcor){
    plot$plotObject$plotFunction <- .corrRedrawWithCaption
    plot$plotObject$plotArgs$caption <- pcorCaption
  }
}

.corrValuePlot <- function(results, cexText= 2.5, cexCI= 1.7, options = options) {
  if(isFALSE(options$scatterPlotStatistic)) return(.displayError(errorMessage = ""))
  if(!isFALSE(results$errors)){
    return(.displayError(errorMessage = gettextf("Correlation undefined: %s", results$errors$message)))
  }

  res   <- results$res
  tests <- c()

  if (options$pearson)      tests <- c(tests, "pearson")
  if (options$spearman)     tests <- c(tests, "spearman")
  if (options$kendallsTauB) tests <- c(tests, "kendall")

  CIPossible <- rep(TRUE, length(tests))

  p <- ggplot2::ggplot(data = data.frame(), ggplot2::aes(x = seq_along(data.frame()),y = summary(data.frame()))) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.margin = grid::unit(c(1,1,1,1), "cm")
    ) + ggplot2::xlim(0,2) + ggplot2::ylim(0,2)

  if(length(tests) == 0){
    return(p)
  } else if(length(tests) == 1){
    ypos <- 1.5
  } else if(length(tests) == 2){
    ypos <- c(1.7, 1.1)
  } else if(length(tests) == 3){
    ypos <- c(1.8, 1.2, .6)
  }

  lab <- rep(NA, length(tests))
  cilab <- rep(NA, length(tests))

  for(i in seq_along(tests)){
    estimate <- res[[tests[i]]][['estimate']]
    if (is.na(estimate)) {
      CIPossible[i] <- FALSE
      lab[i] <- switch(tests[i],
                       pearson =  paste(  "italic(r) == 'NA'"),
                       spearman = paste("italic(rho) == 'NA'"),
                       kendall =  paste("italic(tau) == 'NA'"))
    } else if (round(estimate, 8) == 1) {
      CIPossible[i] <- FALSE

      #no clue as to what is going on down there... Should this be translated?
      lab[i] <- switch(tests[i],
                       pearson =  paste(  "italic(r) == '1.000'"),
                       spearman = paste("italic(rho) == '1.000'"),
                       kendall =  paste("italic(tau) == '1.000'"))
    } else if(round(estimate, 8) == -1){
      CIPossible[i] <- FALSE

      lab[i] <- switch(tests[i],
                       pearson =  paste(  "italic(r) == '-1.000'"),
                       spearman = paste("italic(rho) == '-1.000'"),
                       kendall =  paste("italic(tau) == '-1.000'"))
    } else{
      lab[i] <- .corValueString(corValue = estimate, testType = tests[i], decimals = 3)
    }

    if(CIPossible[i]){
      #these statements here could all be put together in the call to gettextf right? something like %.3d?
      lower.ci <- res[[tests[i]]][['lower.ci']]
      lower.ci <- formatC(lower.ci, format = "f", digits = 3)

      upper.ci <- res[[tests[i]]][['upper.ci']]
      upper.ci <- formatC(upper.ci, format = "f", digits = 3)

      cilab[i] <- gettextf("%1$s%% CI: [%2$s, %3$s]", 100*options$ciLevel, lower.ci, upper.ci)
    } else{
      cilab[i] <- ""
    }
  }

  p <- p + ggplot2::geom_text(data = data.frame(x = rep(1, length(ypos)), y = ypos),
                              mapping = ggplot2::aes(x = x, y = y, label =lab),
                              size = 7, parse = TRUE)

  if(options$ci){
    p <- p + ggplot2::geom_text(data = data.frame(x = rep(1, length(ypos)), y = ypos - 0.25),
                                mapping = ggplot2::aes(x = x, y = y, label = cilab),
                                size = 5)
  }

  return(p)
}

.corrMarginalDistribution <- function(variable, varName, options, xName = NULL, yName = "Density", errors, coord_flip = FALSE){
  if(isFALSE(options$scatterPlotDensity))  return(.displayError(errorMessage = "")) # return empty plot
  if(length(variable) < 3)            return(.displayError(errorMessage = gettext("Plotting not possible:\n Number of observations is < 3")))
  if(any(is.infinite(variable)))      return(.displayError(errorMessage = gettext("Plotting not possible: Infinite value(s)")))


  if(isTRUE(options$plotRanks)) variable <- rank(variable)

  p <- .plotMarginalCor(variable = variable, xName = xName, yName = yName)

  if(coord_flip){
    p <- p + ggplot2::coord_flip() +
      ggplot2::theme(axis.ticks.y = ggplot2::element_line(), axis.ticks.x = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_blank())
  }

  p
}

.corrScatter <- function(xVar, yVar, options, xBreaks = NULL, yBreaks = NULL, xName = NULL, yName = NULL,
                         drawAxes = TRUE) {
  if(length(xVar) <= 1 || length(yVar) <= 1) return(.displayError(errorMessage = gettext("Plotting not possible:\n Number of observations is < 2")))
  errors <- .hasErrors(data.frame(xVar = xVar, yVar = yVar), message = 'short',
                       type = c('infinity'),
                       all.target = c("xVar", "yVar"),
                       exitAnalysisIfErrors = FALSE)
  if(is.list(errors)) return(.displayError(errorMessage = gettextf("Plotting not possible: %s", errors$message)))

  if(isTRUE(options$plotRanks)) {
    xVar <- rank(xVar)
    yVar <- rank(yVar)
  }
  .plotScatter(xVar = xVar, yVar = yVar, options, xBreaks = xBreaks, yBreaks = yBreaks, xName = xName, yName = yName,
               drawAxes = drawAxes)
}

.corrHeatmap <- function(jaspResults, options, corrResults, ready){
  if(isFALSE(options$heatmapPlot)) return()

  hw <- 30 + 80*length(options$variables)

  #TODO: The following looks rather familiar and all these defines should, I think, all be put together in one place instead of scattered throughout this file...
  tests <- c("pearson", "spearman", "kendall")
  if(length(options$partialOutVariables) == 0){
    names(tests) <- c(gettext("Pearson's r"), gettext("Spearman's rho"), gettext("Kendall's tau B"))
  } else{
    names(tests) <- c(gettext("Partial Pearson's r"), gettext("Partial Spearman's rho"), gettext("Partial Kendall's tau B"))
  }
  tests <- tests[c(options$pearson, options$spearman, options$kendallsTauB)]

  if(length(tests) == 0){
    return()
  } else if(length(tests) == 1){
    plot <- createJaspPlot(title = gettextf("%s heatmap", names(tests)), width = hw, height = hw)
    plot$dependOn(c("variables", "partialOutVariables", "naAction", "pearson", "spearman", "kendallsTauB",
                    "significanceFlagged", "heatmapPlot"))
    plot$position <- 4
    jaspResults[['heatmaps']] <- plot

    if(ready) plot$plotObject <- .corrPlotHeatmap(tests, options, corrResults)
  } else{
    heatmaps <- createJaspContainer(title = gettext("Heatmaps"))
    heatmaps$dependOn(c("variables", "partialOutVariables", "naAction", "pearson", "spearman", "kendallsTauB",
                        "significanceFlagged", "heatmapPlot"))
    heatmaps$position <- 4
    jaspResults[['heatmaps']] <- heatmaps

    for(i in seq_along(tests)){
      plot <- createJaspPlot(title = names(tests[i]), width = hw, height = hw)
      heatmaps[[tests[[i]]]] <- plot

      if(ready) plot$plotObject <- .corrPlotHeatmap(tests[[i]], options, corrResults)
    }
  }
}

.corrPlotHeatmap <- function(method, options, corrResults){
  data <- lapply(corrResults, function(x){
    c(var1 = x[['vars']][1], var2 = x[['vars']][2],
      cor = x[['res']][[method]][['estimate']],
      p = x[['res']][[method]][['p.value']])
  })

  data <- do.call(rbind, data)
  data <- rbind(data, data[, c(2, 1, 3, 4)])
  data <- data.frame(data, stringsAsFactors = FALSE)
  data <- rbind(data, data.frame(var1 = options$variables, var2 = options$variables, cor = NA, p = 1))

  data$var1 <- factor(data$var1, levels = options$variables)
  data$var2 <- factor(data$var2, levels = rev(options$variables))
  data$cor <- as.numeric(data$cor)
  data$p <- as.numeric(data$p)

  data$label <- round(data$cor, 3)
  if(options$significanceFlagged){
    data$label <- ifelse(data$p < 0.05 & !is.na(data$cor), paste0(data$label, "*"), data$label)
    data$label <- ifelse(data$p < 0.01 & !is.na(data$cor), paste0(data$label, "*"), data$label)
    data$label <- ifelse(data$p < 0.001 & !is.na(data$cor), paste0(data$label, "*"), data$label)
  }
  p <- ggplot2::ggplot(data, ggplot2::aes(x = var1, y = var2, fill = cor)) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(ggplot2::aes(x = var1, y = var2, label = label), size = 5) +
    ggplot2::scale_fill_gradient2(limits = c(-1, 1), na.value = "white") +
    ggplot2::coord_equal() +
    ggplot2::xlab(NULL) + ggplot2::ylab(NULL) +
    #ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 270, hjust = 0, vjust = 0.5))
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1))

  jaspGraphs::themeJasp(p)
}

## Old plotting (still in use)----
#### histogram with density estimator ####
.plotMarginalCor <- function(variable, xName = NULL, yName = "Density") {

  variable <- na.omit(variable)
	isNumeric <- !(is.factor(variable) || (is.integer(variable) && length(unique(variable)) <= 10))


	if (isNumeric) {
		p <- ggplot2::ggplot(data = data.frame(x = variable))
		h <- hist(variable, plot = FALSE)
  	hdiff <- h$breaks[2L] - h$breaks[1L]
		xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(variable, h$breaks), min.n = 3)
		dens <- h$density
  	yBreaks <- c(0, 1.2*max(h$density))

  	p <- p + ggplot2::geom_histogram(
  		mapping  = ggplot2::aes(x = x, y = ..density..),
  		binwidth = hdiff,
  		fill     = "grey",
  		col      = "black",
  		size     = .3,
  		center   = hdiff / 2,
  		stat     = "bin"
  	) +
  		ggplot2::scale_x_continuous(name = xName, breaks = xBreaks, limits = range(xBreaks))
	} else {

		p <- ggplot2::ggplot(data = data.frame(x = factor(variable)))
		hdiff <- 1L
		xBreaks <- unique(variable)
		yBreaks <- c(0, max(table(variable)))
		p <- p + ggplot2::geom_bar(
			mapping  = ggplot2::aes(x = x),
			fill     = "grey",
			col      = "black",
			size     = .3,
			stat     = "count"
		) +
			ggplot2::scale_x_discrete(name = xName, breaks = xBreaks)
	}

	yLim <- range(yBreaks)

  if (isNumeric) {
  	density <- density(variable)
  	p <- p + ggplot2::geom_line(data = data.frame(x = density$x, y = density$y),
  															mapping = ggplot2::aes(x = x, y = y), lwd = .7, col = "black")
  }

	thm <- ggplot2::theme(
		axis.ticks.y = ggplot2::element_blank(),
		axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = -5, b = 0, l = 0))
	)
  p <- p +
  	ggplot2::scale_y_continuous(name = yName, breaks = yBreaks, labels = c("", ""), limits = yLim) +
  	ggplot2::theme()
  return(jaspGraphs::themeJasp(p) + thm)

}


#### scatterplots ####

# predictions of fitted model
.poly.pred <- function(fit, plot = NULL, line=FALSE, xMin, xMax, lwd) {
    # create function formula
    f <- vector("character", 0)

    for (i in seq_along(coef(fit))) {
        if (i == 1) {
            temp <- paste(coef(fit)[[i]])
            f <- paste(f, temp, sep="")
        }

        if (i > 1) {
            temp <- paste("(", coef(fit)[[i]], ")*", "x^", i-1, sep="")
            f <- paste(f, temp, sep="+")
        }
    }

    x <- seq(xMin, xMax, length.out = 100)
    predY <- eval(parse(text=f))

    if (line == FALSE) {
        return(predY)
    }

    if (line) {
        plot <- plot + ggplot2::geom_line(data = data.frame(x, predY),mapping = ggplot2::aes(x = x, y = predY), size=lwd)
        return(plot)
    }
}

.plotScatter <- function(xVar, yVar, options = NULL, xBreaks = NULL, yBreaks = NULL, xName = NULL, yName = NULL, drawAxes = TRUE) {

	isNumericX <- !(is.factor(xVar) || (is.integer(xVar) && length(unique(xVar)) <= 10))
	isNumericY <- !(is.factor(yVar) || (is.integer(yVar) && length(unique(yVar)) <= 10))
	bothNumeric <- isNumericX && isNumericY
  d <- data.frame(x = xVar, y = yVar)
  d <- na.omit(d)

  if (!isNumericX)
  	d$x <- as.factor(d$x)

  if (!isNumericY)
  	d$y <- as.factor(d$y)

  if (is.null(xBreaks))
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(d$x)

  fit <- NULL

  if (bothNumeric) {

  	fit <- lm(y ~ poly(x, 1, raw = TRUE), d)
  	lineObj <- jaspDescriptives::.poly.predDescriptives(fit, line = FALSE, xMin= xBreaks[1], xMax = xBreaks[length(xBreaks)], lwd = lwd)
  	rangeLineObj <- c(lineObj[1], lineObj[length(lineObj)])
  	yLimits <- range(c(pretty(yVar)), rangeLineObj)

  	if (is.null(yBreaks))
  		yBreaks <- jaspGraphs::getPrettyAxisBreaks(yLimits)

  } else if (is.null(yBreaks)) {

  	yBreaks <- jaspGraphs::getPrettyAxisBreaks(d$y)

  }

  p <- ggplot2::ggplot(data = d, ggplot2::aes(x = x, y = y)) +
    jaspGraphs::geom_point()

  if (bothNumeric) {
  	xr <- seq(from = min(xBreaks), to = max(xBreaks), length.out = length(lineObj))
  	dfLine <- data.frame(x = xr, y = lineObj)
    p <- p + ggplot2::geom_line(data = dfLine, ggplot2::aes(x = x, y = y), size = .7, inherit.aes = FALSE)

    if (isTRUE(options$scatterPlotCi)) {
      ci <- as.data.frame(stats::predict(fit, interval = "confidence", level = options$scatterPlotCiLevel))
      ci[["x"]] <- d$x

      p <- p + ggplot2::geom_line(data = ci, ggplot2::aes(x = x, y = lwr), size = 1, color = "darkblue", linetype = "dashed") +
        ggplot2::geom_line(data = ci, ggplot2::aes(x = x, y = upr), size = 1, color = "darkblue", linetype = "dashed")
    }

    if (isTRUE(options$scatterPlotPredictionInterval)) {
      pi <- as.data.frame(stats::predict(fit, interval = "prediction", level = options$scatterPlotPredictionIntervalLevel))
      pi[["x"]] <- d$x

      p <- p + ggplot2::geom_line(data = pi, ggplot2::aes(x = x, y = lwr), size = 1, color = "darkgreen", linetype = "longdash") +
        ggplot2::geom_line(data = pi, ggplot2::aes(x = x, y = upr), size = 1, color = "darkgreen", linetype = "longdash")
    }
  }

  if(drawAxes){
    if (isNumericX) {
    	p <- p + ggplot2::scale_x_continuous(name = xName, breaks = xBreaks, limits = range(xBreaks))
    } else {
    	p <- p + ggplot2::scale_x_discrete(name = xName)
    }

    if (isNumericY) {
    	p <- p + ggplot2::scale_y_continuous(name = yName, breaks = yBreaks, limits = range(yBreaks))
    } else {
    	p <- p + ggplot2::scale_y_discrete(name = yName)
    }
  } else{
    if (isNumericX) {
      p <- p + ggplot2::scale_x_continuous(name = NULL, breaks = xBreaks, labels = NULL, limits = range(xBreaks))
    } else {
      p <- p + ggplot2::scale_x_discrete(name = NULL)
    }

    if (isNumericY) {
      p <- p + ggplot2::scale_y_continuous(name = NULL, breaks = yBreaks, labels = NULL, limits = range(yBreaks))
    } else {
      p <- p + ggplot2::scale_y_discrete(name = NULL)
    }
  }

  return(jaspGraphs::themeJasp(p))
}

#### display correlation value ####
.plotCorValue <- function(xVar, yVar, cexText= 2.5, cexCI= 1.7, hypothesis = "twoSided", pearson=options$pearson,
                          kendallsTauB=options$kendallsTauB, spearman=options$spearman, confidenceInterval=0.95) {

    CIPossible <- TRUE

    #Again copy-paste from somewhere else. This should be put in the same place as all those other random lists  being made in this analysis
    tests <- c()
    if (pearson)      tests <- c(tests, "pearson")
    if (spearman)     tests <- c(tests, "spearman")
    if (kendallsTauB) tests <- c(tests, "kendall")


    p <- ggplot2::ggplot(data = data.frame(), ggplot2::aes(x = seq_along(data.frame()),y = summary(data.frame()))) +
        ggplot2::theme_void() +
        ggplot2::theme(
            plot.margin = grid::unit(c(1,1,1,1), "cm")
            ) + ggplot2::xlim(0,2) + ggplot2::ylim(0,2)

    lab <- vector("list")

    if (length(tests) == 1) {
        ypos <- 1.5
    }

    if (length(tests) == 2) {
        ypos <- c(1.6, 1.2)
    }

    if (length(tests) == 3) {
        ypos <- c(1.7, 1.2, .7)
    }

    for (i in seq_along(tests)) {
        if (round(cor.test(xVar, yVar, method=tests[i])$estimate, 8) == 1){
            CIPossible <- FALSE

            #Im guessing these statements are related to my confusion at line 840. I guess we could gettext here and there and then it will work but be translated? Unless the user changes language maybe?
            #I'll leave it as is for now JCG 6-1-20
            if(tests[i] == "pearson"){
                lab[[i]] <- paste("italic(r) == '1.000'")
            }

            if(tests[i] == "spearman"){
                lab[[i]] <- paste("italic(rho) == '1.000'")
            }

            if(tests[i] == "kendall"){
                lab[[i]] <- paste("italic(tau) == '1.000'")
            }
        } else if (round(cor.test(xVar, yVar, method=tests[i])$estimate, 8) == -1){
            CIPossible <- FALSE

            if(tests[i] == "pearson"){
                lab[[i]] <- paste("italic(r) == '-1.000'")
            }

            if(tests[i] == "spearman"){
                lab[[i]] <- paste("italic(rho) == '-1.000'")
            }

            if(tests[i] == "kendall"){
                lab[[i]] <- paste("italic(tau) == '-1.000'")
            }
        } else {
            if(tests[i] == "pearson"){
                #lab[[i]] <- paste0("italic(r) == ",as.character(formatC(round(cor.test(xVar, yVar, method=tests[i])$estimate,3), format="f", digits= 3))[1])
                lab[[i]] <- .corValueString(cor.test(xVar, yVar, method=tests[i])$estimate, "pearson", 3) # fix for rounding off decimals
            }

            if(tests[i] == "spearman"){
                #lab[[i]] <- paste0("italic(rho) == ",as.character(formatC(round(cor.test(xVar, yVar, method=tests[i])$estimate,3), format="f", digits= 3)))
                lab[[i]] <- .corValueString(cor.test(xVar, yVar, method=tests[i])$estimate, "spearman", 3) # fix for rounding off decimals
            }

            if(tests[i] == "kendall"){
                #lab[[i]] <- paste0("italic(tau) == ",as.character(formatC(round(cor.test(xVar, yVar, method=tests[i])$estimate,3), format="f", digits= 3)))
                lab[[i]] <- .corValueString(cor.test(xVar, yVar, method=tests[i])$estimate, "kendall", 3) # fix for rounding off decimals
            }
        }
    }

    for(i in seq_along(tests)){
        p <- p + ggplot2::geom_text(data = data.frame(x = rep(1, length(ypos)), y = ypos), mapping = ggplot2::aes(x = x, y = y, label = unlist(lab)), size = 7, parse = TRUE)
    }


    if (hypothesis == "twoSided" & length(tests) == 1 & any(tests == "pearson")) {
        alternative <- "two.sided"
        ctest <- cor.test(xVar, yVar, method= tests, conf.level=confidenceInterval)
    }

    if (hypothesis != "twoSided" & length(tests) == 1 & any(tests == "pearson")) {
        if (hypothesis == "greater") {
            ctest <- cor.test(xVar, yVar, method=tests, alternative="greater", conf.level=confidenceInterval)
        } else if (hypothesis == "less") {
            ctest <- cor.test(xVar, yVar, method=tests, alternative="less", conf.level=confidenceInterval)
        }
    }

    if (any(tests == "pearson")& length(tests) == 1 && CIPossible) {
        CIlow <- formatC(round(ctest$conf.int[1],3), format = "f", digits = 3)
        CIhigh <- formatC(round(ctest$conf.int[2],3), format = "f", digits = 3)

        if(length(p)>0){
            p <- p + ggplot2::geom_text(data = data.frame(x = 1, y = 1.2), mapping = ggplot2::aes(x = x, y = y, label = gettextf("%1$d%% CI: [%2$d, %3$d]", 100 * confidenceInterval, CIlow, CIhigh)), size = 5)
        }

    }

    return(p)

}

### empty Plot with error message ###
.displayError <- function(errorMessage=NULL, cexText=1.6, lwdAxis= 1.2, wrap = 20) {
  if(!is.null(wrap)) errorMessage <- paste(strwrap(errorMessage, wrap), collapse="\n")

    p <- ggplot2::ggplot(data = data.frame(), ggplot2::aes(x = seq_along(data.frame()),y = summary(data.frame()))) +
        ggplot2::theme(
            panel.border = ggplot2::element_blank(),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            panel.background = ggplot2::element_blank(),
            axis.line = ggplot2::element_blank(),
            axis.ticks.y = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank(),
            axis.text.y = ggplot2::element_blank(),
            plot.margin = grid::unit(c(2,1,1,2), "cm"),
            axis.text.x =ggplot2::element_blank(),
            axis.title = ggplot2::element_blank()) +
        ggplot2::annotate("text", x = 0, y = 0, label = errorMessage, size = 5) +
        ggplot2::xlim(-30, 30) +
        ggplot2::ylim(-30, 30)
    return(p)
}

### helpers ----
.corrNormalApproxConfidenceIntervals <- function(obsCor, n, hypothesis="two.sided", confLevel=0.95){
  zCor <- atanh(obsCor)
  se   <- 1/sqrt(n-3)

  alpha <- 1-confLevel

  if(hypothesis == "two.sided"){
    z <- qnorm(p = alpha/2, lower.tail = FALSE)
    upper.ci <- tanh(zCor + z * se)
    lower.ci <- tanh(zCor - z * se)
  } else if(hypothesis == "less"){
    z <- qnorm(p = alpha, lower.tail = FALSE)
    upper.ci <- tanh(zCor + z * se)
    lower.ci <- -1
  } else if(hypothesis == "greater"){
    z <- qnorm(p = alpha, lower.tail = FALSE)
    upper.ci <- 1
    lower.ci <- tanh(zCor - z * se)
  }

  return(c(lower.ci,upper.ci))
}

.createNonparametricConfidenceIntervals <- function(x, y, obsCor, alternative = c("twoSided", "greater", "less"), confLevel = 0.95, method = "kendall"){
  # Based on sections 8.3 and 8.4 of Hollander, Wolfe & Chicken, Nonparametric Statistical Methods, 3e.
  alpha <- 1 - confLevel
  missingIndices <- as.logical(is.na(x) + is.na(y)) # index those values that are missing
  x <- x[!missingIndices] # remove those values
  y <- y[!missingIndices]
  n <- length(x)

  alternative <- match.arg(alternative)

  if (method == "kendall") {
    concordanceSumsVector <- concordanceVector_cpp(x, y)
    sigmaHatSq <- 2 * (n-2) * var(concordanceSumsVector) / (n*(n-1))
    sigmaHatSq <- sigmaHatSq + 1 - (obsCor)^2
    sigmaHatSq <- sigmaHatSq * 2 / (n*(n-1))

    if (alternative == "twoSided"){
      z <- qnorm(alpha/2, lower.tail = FALSE)
    } else if (alternative != "twoSided") {
      z <- qnorm(alpha, lower.tail = FALSE)
    }
    ciLow <- obsCor - z * sqrt(sigmaHatSq)
    ciUp <- obsCor + z * sqrt(sigmaHatSq)
    if (alternative == "greater") {
      ciUp <- 1
    } else if (alternative == "less") {
      ciLow <- -1
    }
  } else if (method == "spearman") {
    stdErr = 1/sqrt(n-3)
    if (alternative == "twoSided") {
      z <- qnorm(alpha/2, lower.tail = FALSE)
    } else if (alternative != "twoSided") {
      z <- qnorm(alpha, lower.tail = FALSE)
    }

    ciLow = tanh(atanh(obsCor) - z * stdErr)
    ciUp = tanh(atanh(obsCor) + z * stdErr)

    if (alternative == "greater") {
      ciUp <- 1
    } else if (alternative == "less") {
      ciLow <- -1
    }
  }
  return(c(ciLow,ciUp))
}

.corValueString <- function(corValue = NULL, testType = NULL, decimals = 3){
    if (testType == "pearson")
      type <- "italic(r)"
    else if (testType == "spearman")
      type <- "italic(rho)"
    else #kendall
      type <- "italic(tau)"

    formattedValue <- formatC(round(corValue, decimals), format = "f", digits = decimals)

    return(paste0(type, ' ~ "=" ~ ', '"', formattedValue, '"'))
}

.corrGetTexts <- function() {
  list(
  footnotes = list(
    VSMPR = gettextf("Vovk-Sellke Maximum <em>p</em>-Ratio: Based on the <em>p</em>-value, the maximum possible odds in favor of H%1$s over H%2$s equals 1/(-e <em>p</em> log(<em>p</em>)) for <em>p</em> %3$s .37 (Sellke, Bayarri, & Berger, 2001).","\u2081","\u2080","\u2264"),
    effectSize = gettext("Effect size (Fisher's z): For large correlation coeffiecients (> 0.9) and small sample sizes (< 10), approximation by Fisher's z transformation becomes less accurate")
  ),
  references = list(
    Sellke_etal_2001 = gettext("Sellke, T., Bayarri, M. J., & Berger, J. O. (2001). Calibration of p Values for Testing Precise Null Hypotheses. The American Statistician, 55(1), p. 62-71.")
  )
  )
}
