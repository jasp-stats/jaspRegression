#
# Copyright (C) 2018 University of Amsterdam
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

RegressionLogisticBayesianInternal <- function(jaspResults, dataset = NULL, options) {
  ready <- length(options$modelTerms) > 0 && options$dependent != ""

  if (ready) {
    dataset <- .reglogisticReadData(dataset, options)
    .reglogisticCheckErrors(dataset, options)
  }

  #### first container
  bayesianLogisticRegContainer <- .bayesianLogisticRegGetModelContainer(jaspResults,
                                                                        position = 1)
  bayesianLogisticRegModel     <- .bayesianLogisticRegGetModel(bayesianLogisticRegContainer,
                                                               dataset,
                                                               options,
                                                               ready)

  if (is.null(bayesianLogisticRegContainer[["modelComparisonTable"]]))
    .bayesianLogisticRegTableModelComparison(jaspResults,
                                             bayesianLogisticRegContainer,
                                             bayesianLogisticRegModel,
                                             options,
                                             position = 1)

  if (options$posteriorSummaryTable || options$posteriorSummaryPlot)
    postSumContainer <- .bayesianLogisticRegGetPosteriorSummaryContainer(jaspResults,
                                                                         bayesianLogisticRegContainer,
                                                                         position = 2)

  if (options$posteriorSummaryTable || options$posteriorSummaryPlot || options$marginalPosteriorPlot)
    postSumModel <- .bayesianLogisticRegGetPosteriorSummary(bayesianLogisticRegContainer,
                                                            bayesianLogisticRegModel,
                                                            dataset,
                                                            options,
                                                            ready)

  if (options$posteriorSummaryTable && is.null(bayesianLogisticRegContainer[["postSumContainer"]][["postSumTable"]]))
    .bayesianLogisticRegTablePosteriorSummary(jaspResults,
                                              postSumContainer,
                                              postSumModel,
                                              bayesianLogisticRegModel,
                                              options,
                                              position = 3)

  if (options$posteriorSummaryPlot && is.null(bayesianLogisticRegContainer[["postSumContainer"]][["postSumPlot"]]))
    .basregPlotPosteriorSummary(postSumContainer, postSumModel, options, position = 4)

  if (options$logPosteriorOddsPlot && is.null(bayesianLogisticRegContainer[["logPosteriorOddsPlot"]]))
    .basregPlotPosteriorLogOdds(bayesianLogisticRegContainer, bayesianLogisticRegModel, options, position = 5)

  if (options$residualsVsFittedPlot && is.null(bayesianLogisticRegContainer[["ResidualsVsFittedPlot"]]))
    .basregresidualsVsFittedPlot(bayesianLogisticRegContainer, bayesianLogisticRegModel, position = 6)

  if (options$modelProbabilitiesPlot && is.null(bayesianLogisticRegContainer[["modelProbabilitiesPlot"]]))
    .bayesianLogisticRegModelProbabilitiesPlot(bayesianLogisticRegContainer, bayesianLogisticRegModel, position = 7)

  if (options$modelComplexityPlot && is.null(bayesianLogisticRegContainer[["modelComplexityPlot"]]))
    .bayesianLogisticRegModelComplexityPlot(bayesianLogisticRegContainer, bayesianLogisticRegModel, position = 8)

  if (options$inclusionProbabilitiesPlot && is.null(bayesianLogisticRegContainer[["inclusionProbabilitiesPlot"]]))
    .basreginclusionProbabilitiesPlot(bayesianLogisticRegContainer, bayesianLogisticRegModel, position = 9)

  if (options$qqPlot && is.null(bayesianLogisticRegContainer[["qqPlot"]]))
    .basregPlotQQ(bayesianLogisticRegContainer, bayesianLogisticRegModel, position = 10)

  if (options$marginalPosteriorPlot && is.null(bayesianLogisticRegContainer[["postDistContainer"]]))
    .basregPlotsPosteriorDistribution(bayesianLogisticRegContainer, postSumModel, bayesianLogisticRegModel, options, position = 11)


  #### second container
  if (options$descriptives && is.null(jaspResults[["descriptivesContainer"]])) {
    descriptivesContainer <- .bayesianLogisticRegGetDescriptivesContainer(jaspResults,
                                                                          position = 2)
    .bayesianLogisticRegTableDescriptives(jaspResults, descriptivesContainer, dataset, options, ready)
  }

  .basregExportResiduals(bayesianLogisticRegContainer, bayesianLogisticRegModel, dataset, options, ready)
}


.bayesianLogisticRegGetModelContainer <- function(jaspResults, position) {
  if (is.null(jaspResults[["bayesianLogisticReg"]])) {
    bayesianLogisticRegContainer <- createJaspContainer()
    bayesianLogisticRegContainer$position <- position
    bayesianLogisticRegContainer$dependOn(c(
      "dependent", "covariates", "factors", "weights", "modelTerms",
      "priorRegressionCoefficients", "gPriorAlpha", "cchPriorAlpha", "cchPriorBeta", "cchPriorS",
      "modelPrior", "betaBinomialParamA", "betaBinomialParamB", "bernoulliParam",
      "wilsonParamLambda", "castilloParamU",
      "samplingMethod", "samples", "numberOfModels", "seed", "setSeed", "numericalAccuracy"
    ))
    jaspResults[["bayesianLogisticReg"]] <- bayesianLogisticRegContainer
  }
  return(jaspResults[["bayesianLogisticReg"]])
}

.bayesianLogisticRegGetDescriptivesContainer <- function(jaspResults, position) {
  descriptivesContainer <- createJaspContainer("Descriptives")
  descriptivesContainer$position <- position
  descriptivesContainer$dependOn(optionsFromObject   = jaspResults[["bayesianLogisticReg"]],
                                 options             = "descriptives")
    jaspResults[["descriptivesContainer"]] <- descriptivesContainer
  return(jaspResults[["descriptivesContainer"]])
}

.bayesianLogisticRegGetPosteriorSummaryContainer <- function(jaspResults, bayesianLogisticRegContainer, position) {
  if (is.null(bayesianLogisticRegContainer[["postSumContainer"]])) {
    postSumContainer <- createJaspContainer(gettext("Posterior Summary"))
    postSumContainer$position <- position
    postSumContainer$dependOn(optionsFromObject   = jaspResults[["bayesianLogisticReg"]],
                              options             = c("summaryType", "posteriorSummaryPlotCiLevel"))
    bayesianLogisticRegContainer[["postSumContainer"]] <- postSumContainer
  }
  return(bayesianLogisticRegContainer[["postSumContainer"]])
}

.bayesianLogisticRegTableModelComparison <- function(jaspResults, bayesianLogisticRegContainer, bayesianLogisticRegModel, options, position) {
  if(options[['dependent']] == "")
    modelComparisonTable <- createJaspTable(gettext("Model Comparison"))
  else
    modelComparisonTable <- createJaspTable(gettextf("Model Comparison - %s", options[['dependent']]))

  modelComparisonTable$position <- position
  modelComparisonTable$dependOn(optionsFromObject   = jaspResults[["bayesianLogisticReg"]],
                                options             = c("bayesFactorType", "bayesFactorOrder",
                                                        "modelsShown", "numModelsShown"))

  modelComparisonTable$addCitation(c(
    "Clyde, M. A. (2018). BAS: Bayesian Adaptive Sampling for Bayesian Model Averaging. (Version 1.5.3)[Computer software].",
    "Clyde, M. A., Ghosh, J., & Littman, M. L. (2011). Bayesian adaptive sampling for variable selection and model averaging. Journal of Computational and Graphical Statistics, 20, 80-101.",
    "Consonni, G., Fouskakis, D., Liseo, B., & Ntzoufras, I. (2018). Prior Distributions for Objective Bayesian Analysis. Bayesian Analysis, 13, 627-679.",
    "Liang, F., Paulo, R., Molina, G., Clyde, M. A., & Berger, J. O. (2008). Mixtures of g Priors for Bayesian Variable Selection. Journal of the American Statistical Association, 103, 410-423."
  ))

  if (options$modelPrior == "wilson") {
    modelComparisonTable$addCitation(
      "Wilson, M. A., Iversen, E. S., Clyde, M. A., Schmidler, S. C., & Schildkraut, J. M. (2010). Bayesian model search and multilevel inference for SNP association studies. The annals of applied statistics, 4(3), 1342."
    )
    modelComparisonTable$addFootnote(gettextf(
"The Wilson model prior corresponds to a beta binomial prior with %1$s = 1 and %2$s = p * %3$s and \
corresponds to an approximate penalization equal to log(%3$s + 1) in log-odds scale for each additional \
covariate added to the model (Consonni et al., 2018; Wilson et al., 2010).", "\u03B1", "\u03B2", "\u03BB"))
  } else if (options$modelPrior == "castillo") {
    modelComparisonTable$addCitation(
      "Castillo, I., Schmidt-Hieber, J., & Van der Vaart, A. (2015). Bayesian linear regression with sparse priors. The Annals of Statistics, 43(5), 1986-2018."
    )
    modelComparisonTable$addFootnote(gettextf(
      "The Castillo model prior corresponds to a beta binomial prior with %1$s = 1 and %2$s = p^u and is suitable \
for sparse regression when there are more covariates than observations (Castillo et al., 2015).", "\u03B1", "\u03B2"))
  }

  if (options$bayesFactorType == "BF10")      bfTitle <- gettext("BF<sub>10</sub>")
  else if (options$bayesFactorType == "BF01") bfTitle <- gettext("BF<sub>01</sub>")
  else                                        bfTitle <- gettext("Log(BF<sub>10</sub>)")

  modelComparisonTable$addColumnInfo(name = "Models",         type = "string", title = gettext("Models"))
  modelComparisonTable$addColumnInfo(name = "priorProbModel", type = "number", title = gettext("P(M)"))
  modelComparisonTable$addColumnInfo(name = "postProbModel",  type = "number", title = gettext("P(M|data)"))
  modelComparisonTable$addColumnInfo(name = "BFM",            type = "number", title = gettext("BF<sub>M</sub>"))
  modelComparisonTable$addColumnInfo(name = "BF",             type = "number", title = bfTitle)
  modelComparisonTable$addColumnInfo(name = "R2",             type = "number", title = gettextf("R%s", "\u00B2"), format = "dp:3")

  if (!is.null(bayesianLogisticRegModel)) {
    generalNote <- NULL
    if (sum(bayesianLogisticRegModel$nuisanceTerms) > 0)
      generalNote <- gettextf("All models include %s.", paste(names(which(bayesianLogisticRegModel$nuisanceTerms)), collapse = ", "))

    if (options$modelsShown == "limited" && options$numModelsShown < length(bayesianLogisticRegModel$which)) {
      if (is.null(generalNote)) {
        s1 <- ""
      } else {
        s1 <- paste0(generalNote, " ") # hopefully all languages want a " " after a full stop.
      }
      generalNote <- gettextf("%sTable displays only a subset of models; to see all models, select \"No\" under \"Limit No. Models Shown\".", s1)
    }

    if (!is.null(generalNote))
      modelComparisonTable$addFootnote(message = generalNote)

    .bayesianLogisticRegFillTableModelComparison(modelComparisonTable, bayesianLogisticRegModel, options)
  }

  bayesianLogisticRegContainer[["modelComparisonTable"]] <- modelComparisonTable
}

.bayesianLogisticRegFillTableModelComparison <- function(modelComparisonTable, bayesianLogisticRegModel, options) {
  nModels <- length(bayesianLogisticRegModel$which)
  if (options$modelsShown == "limited" && options$numModelsShown < nModels)
    nModels <- options$numModelsShown

  allModelIndices <- .bayesianLogisticRegGetModelOrder(bayesianLogisticRegModel, options$bayesFactorOrder)
  modelIndices <- allModelIndices[1:nModels]

  models <- bayesianLogisticRegModel$which[modelIndices]
  modelNames <- .bayesianLogisticRegGetModelNames(bayesianLogisticRegModel, options)[modelIndices]

  # get the Bayes factors for the models
  logMarg <- bayesianLogisticRegModel$logmarg[modelIndices]
  if (options$bayesFactorType == "BF10")
    bayesFactors <- exp(logMarg - logMarg[1])
  else if (options$bayesFactorType == "BF01")
    bayesFactors <- exp(logMarg[1] - logMarg)
  else # logBF10
    bayesFactors <- logMarg - logMarg[1]

  # calculate the BFM for the models
  postProbs <- bayesianLogisticRegModel$postprobs[modelIndices]
  priorProbs <- bayesianLogisticRegModel$priorprobs[modelIndices]
  valid <- is.finite(postProbs)
  BFM <- numeric(length(postProbs))
  BFM[valid] <- (postProbs[valid] / (1 - postProbs[valid])) / (priorProbs[valid] / (1 - priorProbs[valid]))
  BFM[!valid] <- NA

  modelComparisonTable$setData(list(
    Models = modelNames,
    BF = bayesFactors,
    BFM = BFM,
    postProbModel = postProbs,
    R2 = bayesianLogisticRegModel$R2[modelIndices],
    priorProbModel = priorProbs
  ))
}

.bayesianLogisticRegGetModelOrder <- function(bayesianLogisticRegModel, order) {
  # ordered indices based on posterior probabilities of the models
  sortedModels <- order(bayesianLogisticRegModel$postprobs, decreasing = TRUE)

  if (order == "nullModelTop") {
    indexNullModel <- which(sortedModels == 1)
    sortedModels <- c(1, sortedModels[-indexNullModel])
  }

  return(sortedModels)
}

.bayesianLogisticRegGetModelNames <- function(bayesianLogisticRegModel, options) {
  # null model name
  nuisanceTerms <- bayesianLogisticRegModel$nuisanceTerms
  nullModel <- gettext("Null model")
  if (sum(nuisanceTerms) > 0)
    nullModel <- gettextf("Null model (incl. %s)", paste(names(which(nuisanceTerms)), collapse = ", "))

  models <- bayesianLogisticRegModel$which
  modelNames <- character(length(models))

  # generate all model names
  for (i in 1:length(models)) {
    model <- models[[i]]
    if (length(model) == 1) { # only has intercept
      modelNames[i] <- nullModel
      next
    }

    termsToLookup <- .generateLookupTerms(options[["modelTerms"]])

    for (i in seq_along(bayesianLogisticRegModel$which)) {
      modelTermsIndex <- bayesianLogisticRegModel$which[[i]] + 1
      modelWithNames <- bayesianLogisticRegModel$namesx[modelTermsIndex]
      termsVector <- c()
      for (j in seq_along(termsToLookup)) {
        termNames <- names(termsToLookup)
        lookupTerm <- termsToLookup[[j]]
        if (any(grepl(lookupTerm, modelWithNames)))
          termsVector <- c(termsVector, termNames[[j]])
      }
      termsVector <- if (is.null(termsVector)) nullModel else termsVector
      modelNames[i] <- paste(termsVector, collapse = " + ")
    }
  }
  return(modelNames)
}

.generateLookupTerms <- function(modelTerms, includeNuisance = FALSE) {
  termsPermutedList <- list()
  if (includeNuisance) {
    for (i in seq_along(modelTerms)) {
      modelTerm <- modelTerms[[i]]
      termName <- paste(modelTerm[["components"]], collapse = ":")
      termsPermuted <- combinat::permn(unlist(modelTerm[["components"]]))
      termsPermuted <- unlist(lapply(termsPermuted, paste, collapse = ".*"))
      termsPermuted <- paste(termsPermuted, collapse = "|")
      termsPermutedList[[termName]] <- termsPermuted
    }
  }
  else {
    for (i in seq_along(modelTerms)) {
      modelTerm <- modelTerms[[i]]
      if (!modelTerm[["isNuisance"]]) {
        termName <- paste(modelTerm[["components"]], collapse = ":")
        termsPermuted <- combinat::permn(unlist(modelTerm[["components"]]))
        termsPermuted <- unlist(lapply(termsPermuted, paste, collapse = ".*"))
        termsPermuted <- paste(termsPermuted, collapse = "|")
        termsPermutedList[[termName]] <- termsPermuted
      }
    }
  }

  return(termsPermutedList)
}

.bayesianLogisticRegTablePosteriorSummary <- function(jaspResults, postSumContainer, postSumModel,
                                                      bayesianLogisticRegModel, options, position) {
  postSumTable <- createJaspTable(title = gettext("Posterior Summaries of Coefficients"))
  postSumTable$position <- position
  postSumTable$dependOn(optionsFromObject   = jaspResults[["bayesianLogisticReg"]],
                        options             = c("posteriorSummaryTable",
                                                "effectsType",
                                                "bayesFactorType"))

  bfTitle <- gettext("BF<sub>inclusion</sub>")
  if (options$bayesFactorType == "LogBF10")
    bfTitle <- gettext("Log(BF<sub>inclusion</sub>)")

  overtitle <- gettextf("%s%% Credible Interval", format(100*options[["posteriorSummaryPlotCiLevel"]], digits = 3))
  postSumTable$addColumnInfo(name = "coefficient", title = gettext("Coefficient"),   type = "string")
  postSumTable$addColumnInfo(name = "pInclprior",  title = gettext("P(incl)"),       type = "number")
  postSumTable$addColumnInfo(name = "pExclprior",  title = gettext("P(excl)"),       type = "number")
  postSumTable$addColumnInfo(name = "pIncl",       title = gettext("P(incl|data)"),  type = "number")
  postSumTable$addColumnInfo(name = "pExcl",       title = gettext("P(excl|data)"),  type = "number")
  postSumTable$addColumnInfo(name = "BFincl",      title = bfTitle,                  type = "number")
  postSumTable$addColumnInfo(name = "mean",        title = gettext("Mean"),          type = "number")
  postSumTable$addColumnInfo(name = "sd",          title = gettext("SD"),            type = "number")
  postSumTable$addColumnInfo(name = "lowerCri",    title = gettext("Lower"),         type = "number", overtitle = overtitle)
  postSumTable$addColumnInfo(name = "upperCri",    title = gettext("Upper"),         type = "number", overtitle = overtitle)

  if (options[['factors']] != "" && !is.null(options[['factors']])) {
    footnote <- gettextf("Inclusion/exclusion statistics for coefficients within factor(s) %s are the same as the coefficients are always included or excluded together.", paste(options[["factors"]], collapse = ", "))
    postSumTable$addFootnote(footnote)
  }

  if (!is.null(bayesianLogisticRegModel) && !is.null(postSumModel)) {
    footnote <- postSumModel[["footnotes"]]
    if (!is.null(footnote))
      postSumTable$addFootnote(footnote, symbol = gettext("<em>Warning.</em>"))

    .bayesianLogisticRegFillTablePosteriorSummary(postSumTable, postSumModel, bayesianLogisticRegModel, options)
  }

  postSumContainer[["postSumTable"]] <- postSumTable
}

.bayesianLogisticRegFillTablePosteriorSummary <- function(postSumTable, postSumModel, bayesianLogisticRegModel, options) {
  if (options[["effectsType"]] == "allModels") {

    probne0 <- bayesianLogisticRegModel[["probne0"]]
    priorProbs <- bayesianLogisticRegModel[["priorprobsPredictor"]]
    BFinclusion <- bayesianLogisticRegModel[["BFinclusion"]]

    priorExcl <- 1 - priorProbs
    postExcl  <- 1 - probne0

    # set exclusion probabilities for the intercept to 0 to avoid numerical
    # artefacts for the intercept (e.g., 2.2 * 10^-16)
    priorExcl[1L] <- postExcl[1L] <- 0

  } else {

    priorModelProbs <- bayesianLogisticRegModel$priorprobs
    postModelProbs  <- bayesianLogisticRegModel$postprobs
    terms <- attr(bayesianLogisticRegModel$terms, "factors")[-1, , drop = FALSE]
    rownames(terms) <- rownames(terms)
    colnames(terms) <- colnames(terms)
    inclMat <- BAS:::list2matrix.which(bayesianLogisticRegModel)[, -1, drop = FALSE]
    terms <- rbind(terms, matrix(FALSE, nrow = ncol(terms) - nrow(terms), ncol = ncol(terms)))
    diag(terms) <- FALSE
    storage.mode(terms) <- "logical"
    storage.mode(inclMat) <- "logical"
    rownames(terms) <- colnames(terms)
    effectNames <- colnames(terms)


    #include only real model terms (excluding the factor levels)
    termsToLookup <- .generateLookupTerms(options[["modelTerms"]], includeNuisance = TRUE)
    inclMatColInd <- numeric()
    for (i in seq_along(effectNames)) {
      ind <- which(grepl(termsToLookup[[effectNames[[i]]]], colnames(inclMat)))[[1]]
      inclMatColInd <- c(inclMatColInd, ind)
    }

    inclMat <- inclMat[, inclMatColInd]
    colnames(inclMat) <- effectNames

    tmp <- jaspAnova::BANOVAcomputMatchedInclusion(
      effectNames, inclMat, terms, priorModelProbs, postModelProbs
    )

    probne0 <- priorProbs <- BFinclusion <- priorExcl <- postExcl <- numeric()
    for (i in seq_along(effectNames)) {
      if (i == length(effectNames))
        nEffects <- length(bayesianLogisticRegModel$namesx) - inclMatColInd[[i]]
      else
        nEffects <- inclMatColInd[[i+1]] - inclMatColInd[[i]]

      probne0     <- c(probne0, rep(tmp[["postInclProb"]][[i]], nEffects))
      priorProbs  <- c(priorProbs, rep(tmp[["priorInclProb"]][[i]], nEffects))
      BFinclusion <- c(BFinclusion, rep(tmp[["bfIncl"]][[i]], nEffects))
      priorExcl   <- c(priorExcl, rep(tmp[["priorExclProb"]][[i]], nEffects))
      postExcl    <- c(postExcl, rep(tmp[["postExclProb"]][[i]], nEffects))
    }

    probne0     <- c(1 ,probne0)
    priorProbs  <- c(1, priorProbs)
    BFinclusion <- c(1, BFinclusion)
    priorExcl   <- c(0, priorExcl)
    postExcl    <- c(0, postExcl)
  }

  # show BFinclusion for nuisance predictors as 1, rather than NaN
  priorInclIs1 <- is.nan(BFinclusion) | abs(1 - priorProbs) <= sqrt(.Machine$double.eps)
  BFinclusion[priorInclIs1] <- 1

  if (options$bayesFactorType == "LogBF10")
    BFinclusion <- log(BFinclusion)

  nModels <- bayesianLogisticRegModel[["n.models"]]
  coef <- postSumModel[["coef"]]
  coefficients <- postSumModel[["coefficients"]]
  loopIdx <- postSumModel[["loopIdx"]]
  confInt <- postSumModel[["conf95"]]

  topm <- order(-bayesianLogisticRegModel$postprobs)[1:nModels]
  mostComplex <- which.max(lengths(bayesianLogisticRegModel$which)[topm])

  for (i in loopIdx) {
    coefficient <- coefficients[i]
    pIncl <- probne0[i]
    pInclprior <- priorProbs[i]
    BFincl <- BFinclusion[i]

    if (options$summaryType == "complex") {
      mean <- unname(coef$conditionalmeans[mostComplex, i])
      sd <- unname(coef$conditionalsd[mostComplex, i])
    } else {
      mean <- coef$postmean[i]
      sd <- coef$postsd[i]
    }
    lowerCri <- confInt[i, 1]
    upperCri <- confInt[i, 2]

    row <- list(coefficient = coefficient, mean = mean, sd = sd, pIncl = pIncl,
      pInclprior = pInclprior, BFincl = BFincl, lowerCri = lowerCri, upperCri = upperCri,
      pExclprior = priorExcl[i], pExcl = postExcl[i]
    )
    postSumTable$addRows(row)
  }
}

.basregPlotPosteriorSummary <- function(postSumContainer, postSumModel, options, position) {
  title <- gettextf("Posterior Coefficients with %s%% Credible Interval",
                   format(100 * options$posteriorSummaryPlotCiLevel, digits = 3))
  postSumPlot <- createJaspPlot(title = title, width = 530, height = 400)
  postSumPlot$position <- position
  postSumPlot$dependOn(c("posteriorSummaryPlot", "posteriorSummaryPlotWithoutIntercept"))

  postSumContainer[["postSumPlot"]] <- postSumPlot

  if (!is.null(postSumModel))
    .basregFillPlotPosteriorSummary(postSumPlot, postSumModel, options)
}

.basregFillPlotPosteriorSummary <- function(postSumPlot, postSumModel, options) {
  coef <- postSumModel[["coef"]]
  confInt <- postSumModel[["conf95"]]
  loopIdx <- postSumModel[["loopIdx"]]
  coefficients <- postSumModel[["coefficients"]]
  coefficients <- .basregReplaceInteractionUnicodeSymbol(coefficients)

  # exlude intercept if it's not the only predictor?
  if (options[["posteriorSummaryPlotWithoutIntercept"]] && length(loopIdx) > 1)
    loopIdx <- loopIdx[-1, drop = FALSE]

  confInt <- confInt[loopIdx, , drop = FALSE] # only plot parameters present in table
  df <- data.frame(
    x = factor(coefficients[loopIdx], levels = coefficients[loopIdx]),
    y = confInt[, 3],
    lower = confInt[, 1],
    upper = confInt[, 2]
  )

  p <- try({
    yBreaks <- jaspGraphs::getPrettyAxisBreaks(range(c(confInt)))
    g <- ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = x, y = y, ymin = lower, ymax = upper)) +
      ggplot2::geom_point(size = 4) +
      ggplot2::geom_errorbar(, width = 0.2) +
      ggplot2::scale_x_discrete(name = "") +
      ggplot2::scale_y_continuous(name = expression(beta), breaks = yBreaks, limits = range(yBreaks))
    jaspGraphs::themeJasp(g) +
      ggplot2::theme(
        axis.title.y = ggplot2::element_text(angle = 0, vjust = .5, size = 20)
      )
  })

  if (isTryError(p)) {
    errorMessage <- gettextf("Plotting not possible: %s", .extractErrorMessage(p))
    postSumPlot$setError(errorMessage)
  } else {
    postSumPlot$plotObject <- p
  }
}

.basregPlotPosteriorLogOdds <- function(bayesianLogisticRegContainer, bayesianLogisticRegModel, options, position) {
  postLogOddsPlot <- createJaspPlot(title = gettext("Posterior Log Odds"), width = 530, height = 400)
  postLogOddsPlot$position <- position
  postLogOddsPlot$dependOn("logPosteriorOddsPlot")

  bayesianLogisticRegContainer[["logPosteriorOddsPlot"]] <- postLogOddsPlot

  if (options$samplingMethod == "mcmc") {
    postLogOddsPlot$setError(gettext("Cannot display Posterior Log Odds when sampling method is MCMC."))
    return(postLogOddsPlot)
  }

  if (!is.null(bayesianLogisticRegModel))
    .basregFillPlotPosteriorLogOdds(postLogOddsPlot, bayesianLogisticRegModel)
}

.basregFillPlotPosteriorLogOdds <- function(postLogOddsPlot, bayesianLogisticRegModel) {
  bayesianLogisticRegModel$namesx <- .basregReplaceInteractionUnicodeSymbol(bayesianLogisticRegModel$namesx)
  postLogOddsPlot$plotObject <- function() {
    BAS:::image.bas(bayesianLogisticRegModel, rotate = FALSE)
  }
}


.basregresidualsVsFittedPlot <- function(bayesianLogisticRegContainer, bayesianLogisticRegModel, position) {
  residualsVsFittedPlot <- createJaspPlot(title = gettext("Residuals vs Fitted"), width = 530, height = 400)
  residualsVsFittedPlot$position <- position
  residualsVsFittedPlot$dependOn("residualsVsFittedPlot")

  bayesianLogisticRegContainer[["ResidualsVsFittedPlot"]] <- residualsVsFittedPlot

  if (!is.null(bayesianLogisticRegModel))
    .basregFillresidualsVsFittedPlot(residualsVsFittedPlot, bayesianLogisticRegModel)

}

.basregFillresidualsVsFittedPlot <- function(residualsVsFittedPlot, bayesianLogisticRegModel) {
  x <- fitted(bayesianLogisticRegModel, estimator = "BMA")
  y <- bayesianLogisticRegModel$Y - x
  dfPoints <- data.frame(
    x = x,
    y = y
  )

  p <- try({
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(dfPoints[["x"]], 3)
    g <- jaspGraphs::drawAxis()
    g <- g + ggplot2::geom_hline(yintercept = 0, linetype = 2, col = "gray")
    g <- jaspGraphs::drawPoints(g, dat = dfPoints, size = 2, alpha = .85)
    g <- jaspGraphs::drawSmooth(g, dat = dfPoints, color = "red", alpha = .7) +
      ggplot2::ylab("Residuals") +
      ggplot2::scale_x_continuous(name = gettext("Predictions under BMA"), breaks = xBreaks, limits = range(xBreaks))
    jaspGraphs::themeJasp(g)
  })

  if (isTryError(p)) {
    errorMessage <- gettextf("Plotting not possible: %s", .extractErrorMessage(p))
    residualsVsFittedPlot$setError(errorMessage)
  } else {
    residualsVsFittedPlot$plotObject <- p
  }
}

.bayesianLogisticRegModelProbabilitiesPlot <- function(bayesianLogisticRegContainer, bayesianLogisticRegModel, position) {
  modelProbabilitiesPlot <- createJaspPlot(title = gettext("Model Probabilities"), width = 530, height = 400)
  modelProbabilitiesPlot$position <- position
  modelProbabilitiesPlot$dependOn("modelProbabilitiesPlot")

  bayesianLogisticRegContainer[["modelProbabilitiesPlot"]] <- modelProbabilitiesPlot

  if (!is.null(bayesianLogisticRegModel))
    .basregFillmodelProbabilitiesPlot(modelProbabilitiesPlot, bayesianLogisticRegModel)
}

.basregFillmodelProbabilitiesPlot <- function(modelProbabilitiesPlot, bayesianLogisticRegModel) {
  cum.prob = cumsum(bayesianLogisticRegModel$postprobs)
  m.index = 1:bayesianLogisticRegModel$n.models

  dfPoints <- data.frame(
    x = m.index,
    y = cum.prob
  )

  p <- try({
    xBreaks <- round(seq(1, bayesianLogisticRegModel$n.models, length.out = min(5, bayesianLogisticRegModel$n.models)))
    g <- jaspGraphs::drawSmooth(dat = dfPoints, color = "red", alpha = .7)
    g <- jaspGraphs::drawPoints(g, dat = dfPoints, size = 4) +
      ggplot2::scale_y_continuous(name = gettext("Cumulative Probability"), limits = 0:1) +
      ggplot2::scale_x_continuous(name = gettext("Model Search Order"), breaks = xBreaks)
    jaspGraphs::themeJasp(g)
  })

  if (isTryError(p)) {
    errorMessage <- gettextf("Plotting not possible: %s", .extractErrorMessage(p))
    modelProbabilitiesPlot$setError(errorMessage)
  } else {
    modelProbabilitiesPlot$plotObject <- p
  }
}

.bayesianLogisticRegModelComplexityPlot <- function(bayesianLogisticRegContainer, bayesianLogisticRegModel, position) {
  modelComplexityPlot <- createJaspPlot(title = gettext("Log(P(data|M)) vs. Model Size"), width = 530, height = 400)
  modelComplexityPlot$position <- position
  modelComplexityPlot$dependOn("modelComplexityPlot")

  bayesianLogisticRegContainer[["modelComplexityPlot"]] <- modelComplexityPlot

  if (!is.null(bayesianLogisticRegModel))
    .basregFillmodelComplexityPlot(modelComplexityPlot, bayesianLogisticRegModel)
}

.basregFillmodelComplexityPlot <- function(modelComplexityPlot, bayesianLogisticRegModel) {
  logmarg = bayesianLogisticRegModel$logmarg
  dim = bayesianLogisticRegModel$size

  dfPoints <- data.frame(
    x = dim,
    y = logmarg
  )

  p <- try({
    # gonna assume here that dim (the number of parameters) is always an integer
    xBreaks <- unique(round(pretty(dim)))
    yBreaks <- jaspGraphs::getPrettyAxisBreaks(range(logmarg))
    g <- jaspGraphs::drawPoints(dat = dfPoints, size = 4) +
      ggplot2::scale_y_continuous(name = gettext("Log(P(data|M))"),  breaks = yBreaks, limits = range(yBreaks)) +
      ggplot2::scale_x_continuous(name = gettext("Model Dimension"), breaks = xBreaks)
    jaspGraphs::themeJasp(g)
  })

  if (isTryError(p)) {
    errorMessage <- gettextf("Plotting not possible: %s", .extractErrorMessage(p))
    modelComplexityPlot$setError(errorMessage)
  } else {
    modelComplexityPlot$plotObject <- p
  }
}

.basreginclusionProbabilitiesPlot <- function(bayesianLogisticRegContainer, bayesianLogisticRegModel, position) {
  inclusionProbabilitiesPlot <- createJaspPlot(title = gettext("Inclusion Probabilities"), width = 700, height = 400)
  inclusionProbabilitiesPlot$position <- position
  inclusionProbabilitiesPlot$dependOn("inclusionProbabilitiesPlot")

  bayesianLogisticRegContainer[["inclusionProbabilitiesPlot"]] <- inclusionProbabilitiesPlot

  if (!is.null(bayesianLogisticRegModel))
    .basregFillinclusionProbabilitiesPlot(inclusionProbabilitiesPlot, bayesianLogisticRegModel)
}

.basregFillinclusionProbabilitiesPlot <- function(inclusionProbabilitiesPlot, bayesianLogisticRegModel) {
  probne0 <- bayesianLogisticRegModel$probne0[-1]
  variables <- bayesianLogisticRegModel$namesx[-1] # 1:bayesianLogisticRegModel$n.vars
  variables <- .basregReplaceInteractionUnicodeSymbol(variables)
  priorProb <- bayesianLogisticRegModel$priorprobsPredictor[1:bayesianLogisticRegModel$n.vars][-1]

  # reorder from low to high
  o <- order(probne0, decreasing = FALSE)
  probne0 <- probne0[o]
  variables <- variables[o]
  priorProb <- priorProb[o]

  width <- .8 # width of the bars
  dfBar <- data.frame(
    x = factor(variables, levels = variables),
    y = probne0
  )
  dfLine <- data.frame(
    x = rep(1:(bayesianLogisticRegModel$n.vars-1), each = 2) + c(-width/2, width/2),
    y = rep(priorProb, each = 2),
    g = rep(factor(variables), each = 2),
    g0 = factor(1)
  )
  base <- .1

  p <- try({
    yLimits <- c(0, base * ceiling(max(c(priorProb, probne0)) / base))
    yBreaks <- seq(yLimits[1], yLimits[2], length.out = 5)

    g <- ggplot2::ggplot(data = dfBar, mapping = ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_bar(width = width, stat = "identity", fill = "gray80", show.legend = FALSE)
    g <- jaspGraphs::drawLines(g, dat = dfLine,
                               mapping = ggplot2::aes(x = x, y = y, group = g, linetype = g0), show.legend = TRUE) +
      ggplot2::scale_y_continuous(gettext("Marginal Inclusion Probability"), breaks = yBreaks, limits = yLimits) +
      ggplot2::xlab("") +
      ggplot2::scale_linetype_manual(name = "", values = 2, labels = gettext("Prior\nInclusion\nProbabilities"))

    jaspGraphs::themeJasp(g, horizontal = TRUE, legend.position = "right") +
      ggplot2::theme(
        legend.title = ggplot2::element_text(size = .8*jaspGraphs::graphOptions("fontsize"))
      )
  })

  if (isTryError(p)) {
    errorMessage <- gettextf("Plotting not possible: %s", .extractErrorMessage(p))
    inclusionProbabilitiesPlot$setError(errorMessage)
  } else {
    inclusionProbabilitiesPlot$plotObject <- p
  }
}

.basregPlotQQ <- function(bayesianLogisticRegContainer, bayesianLogisticRegModel, position) {
  qqPlot <- createJaspPlot(title = gettext("Q-Q Plot"), width = 700, height = 400)
  qqPlot$position <- position
  qqPlot$dependOn("qqPlot")

  bayesianLogisticRegContainer[["qqPlot"]] <- qqPlot

  if (!is.null(bayesianLogisticRegModel))
    .basregFillPlotQQ(qqPlot, bayesianLogisticRegModel)
}

.basregFillPlotQQ <- function(qqPlot, bayesianLogisticRegModel) {
  p <- try({
    x <- fitted(bayesianLogisticRegModel, estimator = "BMA")
    y <- bayesianLogisticRegModel$Y - x
    jaspGraphs::plotQQnorm(y)
  })

  if (isTryError(p)) {
    errorMessage <- gettextf("Plotting not possible: %s", .extractErrorMessage(p))
    qqPlot$setError(errorMessage)
  } else {
    qqPlot$plotObject <- p
  }
}

.basregPlotsPosteriorDistribution <- function(bayesianLogisticRegContainer, postSumModel, bayesianLogisticRegModel, options, position) {
  postDistContainer <- createJaspContainer(gettext("Marginal Posterior Distributions")) #TODO: check if this name is ok
  postDistContainer$position <- position
  postDistContainer$dependOn(c(
    "marginalPosteriorPlot", "summaryType",
    "posteriorSummaryPlotCiLevel", "numericalAccuracy", "seed", "setSeed"
  )) #TODO: check if dependencies are correct for this item: was probably wrong in release

  .basregInsertPosteriorDistributionPlots("placeholders", postDistContainer, plotNames, options, bayesianLogisticRegModel)

  if (!is.null(bayesianLogisticRegModel) && !is.null(postSumModel))
      .basregInsertPosteriorDistributionPlots("fill", postDistContainer, plotNames, options, bayesianLogisticRegModel, postSumModel)

  bayesianLogisticRegContainer[["postDistContainer"]] <- postDistContainer
}

.basregInsertPosteriorDistributionPlots <- function(type, postDistContainer, plotNames, options, bayesianLogisticRegModel = NULL, postSumModel = NULL) {
  if (is.null(bayesianLogisticRegModel)) {
    plotNames <- "Intercept"
    isNuisance <- setNames(FALSE, "Intercept")
  } else {
    plotNames <- .basregReplaceInteractionUnicodeSymbol(bayesianLogisticRegModel$namesx)
    isNuisance <- bayesianLogisticRegModel[["nuisanceTerms"]]
    names(isNuisance) <- .basregReplaceInteractionUnicodeSymbol(names(isNuisance))
  }

  for (plotName in plotNames) {
    if (bayesianLogisticRegModel[["priorprobsPredictor"]][which(plotNames == plotName)] == 1)
      next

    if (type == "placeholders")
      postDistContainer[[plotName]] <- createJaspPlot(title = plotName, width = 530, height = 400)
    else
      .basregFillPlotPosteriorDistribution(postDistContainer[[plotName]], which(plotNames == plotName), postSumModel)

  }
}

.basregFillPlotPosteriorDistribution <- function(posteriorDistributionPlot, index, postSumModel) {
  # these first lines are there to create compatibility with the BAS plotting code we copied
  x      <- postSumModel[["coefBMA"]]
  conf95 <- postSumModel[["conf95BMA"]]
  subset <- list(index)
  e      <- 1e-04

  # based on BAS:::plot.coef.bas.
  # start of copied code
  df <- x$df
  i  <- index

  sel      <- x$conditionalmeans[, i] != 0
  prob0    <- 1 - x$probne0[i]
  mixprobs <- x$postprobs[sel]/(1 - prob0)
  means    <- x$conditionalmeans[sel, i, drop = TRUE]
  sds      <- x$conditionalsd[sel, i, drop = TRUE]
  name     <- x$namesx[i]
  name     <- .basregReplaceInteractionUnicodeSymbol(name)
  df.sel   <- df[sel]

  df <- df.sel # modified from original

  p <- try(silent = FALSE, expr = {
    nsteps <- 500
    if (prob0 == 1 | length(means) == 0) {
      xlower <- -0
      xupper <- 0
      xmax   <- 1
    } else {
      qmin <- min(qnorm(e/2, means, sds))
      qmax <- max(qnorm(1 - e/2, means, sds))
      if (i > 1) {
        xlower <- min(qmin, 0)
        xupper <- max(0, qmax)
      } else {
        xlower <- qmin
        xupper <- qmax
      }
    }

    xx    <- seq(xlower, xupper, length.out = nsteps)
    yy    <- rep(0, times = length(xx))
    maxyy <- 1
    if (prob0 < 1 & length(sds) > 0) {
      yy <- mixprobs %*% apply(matrix(xx, ncol = 1), 1,
                              FUN = function(x, d, m, s) {
                                dt(x = (x - m)/s, df = d)/s
                              }, d = df, m = means, s = sds)
      maxyy <- max(yy)
    }
    ymax <- max(prob0, 1 - prob0)
    # end of copied code

    dens    <- (1 - prob0) * yy/maxyy
    dfLines <- data.frame(
      x = c(0, 0, xx),
      y = c(0, prob0, dens),
      g = factor(rep(1:2, c(2, length(xx))))
    )

    xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(xlower, xupper))
    yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, 1.15*max(dfLines$y)))

    # figure out whether to draw text left or right of 0
    step      <- (xupper + abs(xlower)) / (nsteps - 1)  # stepsize of grid
    idx0      <- round(abs(xlower / step))              # idx of x closest to 0
    idxMax    <- which.max(dens)                      # idx of maximum of density
    maxX      <- xx[idxMax]                             # x value at maximum of density
    maxHeight <- dens[idxMax]                        # y value at maximum of density
    if (prob0 > maxHeight) { # if text drawn above posterior no action is required

      xText <- 0.05 * xBreaks[length(xBreaks)]
      hjust <- "left"
      # text below maxheight

    } else {

      # text is drawn right if:
      # - density is below textheight
      # - peak of density is left of textheight

      # text drawn at similar height as posterior
      if (maxX < 0 && dens[idx0] < prob0) {
        # peak is left of text; density is below text height
        xText <- 0.05 * xBreaks[length(xBreaks)]
        hjust <- "left"

      } else {

        xText <- -abs(0.05 * xBreaks[1])
        hjust <- "right"

      }

    }
    dfText <- data.frame(
      x = xText,
      y = prob0,
      label = format(prob0, digits = 3, scientific = -2)
    )

    # obtain credible interval given that predictor is in model
    cri <- conf95[i, 1:2]
    # find closest x-locations on grid to credible interval
    idxCri <- c(
      which.min(abs(xx - cri[1])),
      which.min(abs(xx - cri[2]))
    )
    dfCri <- data.frame(
      xmin = xx[idxCri[1]],
      xmax = xx[idxCri[2]],
      y = 0.9 * yBreaks[length(yBreaks)]
    )
    hBarHeight <- 0.05 * yBreaks[length(yBreaks)]
    dfCriText <- data.frame(
      x = xx[idxCri],
      y = 0.975 * yBreaks[length(yBreaks)],
      label = format(cri, digits = 3, scientific = -2)
    )

    g <- ggplot2::ggplot(data = dfLines, mapping = ggplot2::aes(x = x, y = y, group = g, color = g)) +
      ggplot2::geom_line(size = 1.25, show.legend = FALSE) +
      ggplot2::scale_y_continuous(name = "Density", breaks = yBreaks, limits = range(yBreaks)) +
      ggplot2::scale_x_continuous(name = name, breaks = xBreaks, limits = range(xBreaks)) +
      ggplot2::scale_color_manual(values = c("gray", "black"))
    if (prob0 > 0.01)
      g <- g + ggplot2::geom_text(data = dfText, mapping = ggplot2::aes(x = x, y = y, label = label),
                                  size = 6, hjust = hjust, inherit.aes = FALSE)
    g <- g + ggplot2::geom_errorbarh(data = dfCri, mapping = ggplot2::aes(xmin = xmin, xmax = xmax, y = y),
                                     height = hBarHeight, inherit.aes = FALSE) +
      ggplot2::geom_text(data = dfCriText, mapping = ggplot2::aes(x = x, y = y, label = label), size = 6,
                         hjust = c("right", "left"), inherit.aes = FALSE)

    jaspGraphs::themeJasp(g)
  })

  if (isTryError(p)) {
    errorMessage <- gettextf("Plotting not possible: %s", .extractErrorMessage(p))
    posteriorDistributionPlot$setError(errorMessage)
  } else {
    posteriorDistributionPlot$plotObject <- p
  }
}

.bayesianLogisticRegTableDescriptives <- function(jaspResults, descriptivesContainer, dataset, options, ready) {
  factorDescriptivesTable <- createJaspTable(title = gettext("DV/Factor Descriptives"))
  factorDescriptivesTable$position <- 1
  factorDescriptivesTable$dependOn(c("descriptives", "factors", "dependent"))

  factorDescriptivesTable$addColumnInfo(name = "Factor", title = gettext("DV/Factor"), type = "string")
  factorDescriptivesTable$addColumnInfo(name = "Level",  title = gettext("Level"),  type = "string")
  factorDescriptivesTable$addColumnInfo(name = "N",      title=gettext("N"),        type = "integer")

  jaspResults[["descriptivesContainer"]][["factorDescriptivesTable"]] <- factorDescriptivesTable
  if (ready) {
    .bayesianLogisticRegFillTableFactorDescriptives(factorDescriptivesTable, dataset, options)
    jaspResults[["descriptivesContainer"]][["factorDescriptivesTable"]] <- factorDescriptivesTable
  }

  if (ready && (length(options$covariates) > 0)) {
    covariateDescriptivesTable <- createJaspTable(title = gettext("Covariate Descriptives"))
    covariateDescriptivesTable$position <- 2
    covariateDescriptivesTable$dependOn(c("descriptives", "covariates"))

    covariateDescriptivesTable$addColumnInfo(name = "v",     title = "",              type = "string")
    covariateDescriptivesTable$addColumnInfo(name = "N",     title = gettext("N"),    type = "integer")
    covariateDescriptivesTable$addColumnInfo(name = "mean",  title = gettext("Mean"), type = "number")
    covariateDescriptivesTable$addColumnInfo(name = "sd",    title = gettext("SD"),   type = "number")

    .bayesianLogisticRegFillTableCovariateDescriptives(covariateDescriptivesTable, dataset, options)
    jaspResults[["descriptivesContainer"]][["covariateDescriptivesTable"]] <- covariateDescriptivesTable
  }

}

.bayesianLogisticRegFillTableCovariateDescriptives <- function(descriptivesTable, dataset, options) {
  discreteVariables <- c(options$dependent, unlist(options$factors))

  variables <- unlist(options$covariates)
  if (length(variables) > 0) {
    for (variable in variables) {
      data <- na.omit(dataset[[variable]])
      n <- length(data)
      mean <- mean(data)
      sd <- sd(data)

      descriptivesTable$addRows(list(v = variable, N = n, mean = mean, sd = sd))
    }
  }
}

.bayesianLogisticRegFillTableFactorDescriptives <- function(descriptivesTable, dataset, options) {
  variables <- c(options$dependent, unlist(options$factors))

  if (length(variables) > 0) {
    for (variable in variables) {
      data <- na.omit(dataset[[variable]])
      counts <- table(data)
      factorLevels <- names(counts)
      for (levelIndex in seq(factorLevels)) {
        if (levelIndex == 1)
          descriptivesTable$addRows(list(Factor = variable,
                                         Level = factorLevels[[levelIndex]],
                                         N = counts[[levelIndex]]))
        else
          descriptivesTable$addRows(list(Factor = "",
                                         Level = factorLevels[[levelIndex]],
                                         N = counts[[levelIndex]]))
      }
    }
  }
}


.bayesianLogisticRegGetModel <- function(bayesianLogisticRegContainer, dataset, options, ready) {
  if (!ready || bayesianLogisticRegContainer$getError())
    return()

  if (!is.null(bayesianLogisticRegContainer[["bayesianLogisticRegModel"]])) {
    # see https://github.com/jasp-stats/INTERNAL-jasp/issues/1263
    # when objects are loaded from the state, no BAS:: methods are called.
    # as a consequence, fitted may not dispatch to BAS::fitted.bas (same for other S3 methods)
    loadNamespace("BAS")
    return(bayesianLogisticRegContainer[["bayesianLogisticRegModel"]]$object)
  }

  formulas <- .bayesianLogisticRegCreateFormula(options[["dependent"]], options[["modelTerms"]])
  modelFormula <- formulas[["modelFormula"]]
  nullFormula <- formulas[["nullFormula"]]
  isNuisance <- .bayesianLogisticRegCreateNuisanceLookupVector(options$modelTerms)
  nPreds <- length(options$modelTerms)

  # set initprobs (BAS' method for nuisance terms)
  initProbs <- rep(0.5, nPreds + 1) # the + 1 is the intercept
  index <- c(1, which(isNuisance) + 1)
  initProbs[index] <- 1

  # get the weights
  weights <- NULL
  if (options$weights != "") {
    weightsVar <- options$weights
    weights <- dataset[[weightsVar]]
  }

  # select the type of model prior
  if (options$modelPrior == "betaBinomial")
    modelPrior <- BAS::beta.binomial(as.numeric(options$betaBinomialParamA), as.numeric(options$betaBinomialParamB))
  else if (options$modelPrior == "uniform")
    modelPrior <- BAS::uniform()
  else if (options$modelPrior == "bernoulli")
    modelPrior <- BAS::Bernoulli(options$bernoulliParam)
  else if (options$modelPrior == "wilson")
    modelPrior <- BAS::beta.binomial(1.0, as.numeric(nPreds * options$wilsonParamLambda))
  else if (options$modelPrior == "castillo")
    modelPrior <- BAS::beta.binomial(1.0, as.numeric(nPreds ^ options$castilloParamU))

  # number of models
  n.models <- NULL
  if (options$samplingMethod == "bas" && options$numberOfModels > 0)
    n.models <- options$numberOfModels

  # iterations for MCMC
  MCMC.iterations <- NULL

  # convert QML input to prior value that bas.lm expects
  prior <- switch(
    options$priorRegressionCoefficients,
    "aic"           = BAS::aic.prior(),
    "betaPrime"     = BAS::beta.prime(),
    "bic"           = BAS::bic.prior(),
    "ebLocal"       = BAS::EB.local(),
    "cch"           = BAS::CCH(alpha = options$cchPriorAlpha,
                               beta  = options$cchPriorBeta,
                               s     = options$cchPriorS),
    "gPrior"        = BAS::g.prior(g = options$gPriorAlpha),
    "instrinsic"    = BAS::intrinsic(),
    "robust"        = BAS::robust()
  )

  # Bayesian Adaptive Sampling
  .setSeedJASP(options)
  basGlmObject <- try(BAS::bas.glm(
    formula         = modelFormula,
    family          = binomial(link = "logit"),
    data            = dataset,
    weights         = weights,
    betaprior       = eval(prior, envir = parent.frame()),
    modelprior      = modelPrior,
    n.models        = n.models,
    method          = toupper(options$samplingMethod),
    MCMC.iterations = MCMC.iterations,
    renormalize     = TRUE,
    force.heredity  = TRUE,
    include.always  = nullFormula
  ))

  if (isTryError(basGlmObject)) {
    errorMsg <- .extractErrorMessage(basGlmObject)
    bayesianLogisticRegContainer$setError(errorMsg)
    return()
  }

  if (basGlmObject$n.models > 1 && nPreds > 1) # can crash without this check
    basGlmObject <- BAS::force.heredity.bas(basGlmObject)

  # fix for prior probs all returning 1 with uniform and bernoulli 0.5 priors
  basGlmObject[["priorprobs"]] <- basGlmObject[["priorprobs"]] / sum(basGlmObject[["priorprobs"]])
  basGlmObject[["priorprobsPredictor"]] <- .bayesianLogisticRegComputePriorMarginalInclusionProbs(basGlmObject)
  basGlmObject[["weights"]] <- weights
  basGlmObject[["BFinclusion"]] <- .bayesianLogisticRegComputeInclusionBF(basGlmObject)
  basGlmObject[["namesx"]][-1] <- basGlmObject[["namesx"]][-1]
  basGlmObject[["namesx"]] <- .bayesianLogisticRegRenameTermsWithLevels(basGlmObject[["namesx"]], options[["covariates"]], options[["factors"]])
  basGlmObject[["nuisanceTerms"]] <- setNames(isNuisance, names(isNuisance))

  bayesianLogisticRegContainer[["bayesianLogisticRegModel"]] <- createJaspState(basGlmObject)

  return(basGlmObject)
}

.bayesianLogisticRegRenameTermsWithLevels <- function(names, covariates, factors) {
  # This function renames model coefficients such that factor levels are formatted nicely
  # e.g. "facExperim (experimental)" instead of "facExperimexperimental"
  # The code is horrible, I could not figure out atm how to do it more elegantly
  # (or is there a jaspBase function that does that??)
  # It would be good to make this better at some point
  predNames <- c(gettext("Intercept"), covariates, factors)

  componentsList <- strsplit(names, ":")

  newNames <- c()
  for (components in componentsList) {
    newName <- NULL
    for (component in components) {
      for (predName in predNames) {
        if(grepl(predName, component)) {
          niceLevel <- gsub(predName, "", component)
          niceLevel <- if(niceLevel == "") "" else sprintf(" (%s)", niceLevel)
          newName <- paste(c(newName, paste0(predName, niceLevel)), collapse = ":")
          break
        }
      }
    }
    newNames <- c(newNames, newName)
  }

  return(newNames)
}

.bayesianLogisticRegCreateFormula <- function(dependent, modelTerms) {
  modelFormula <- c(dependent, "~ 1")
  nullFormula <- c("~ 1")
  for (i in 1:length(modelTerms)) {
    term <- modelTerms[[i]]
    termName <- paste(term$component, collapse = "*")
    modelFormula <- c(modelFormula, "+", termName)
    if (term[["isNuisance"]]) nullFormula <- c(nullFormula, "+", termName)
  }
  modelFormula <- as.formula(paste(modelFormula, collapse = ""), env = parent.frame(1)) # bas.lm searches for objects defined in .basregGetModel in the formula env..
  nullFormula <- as.formula(paste(nullFormula, collapse = ""), env = parent.frame(1))
  return(list("modelFormula" = modelFormula, "nullFormula" = nullFormula))
}

.bayesianLogisticRegCreateNuisanceLookupVector <- function(modelTerms) {
  isNuisance <- rep(FALSE, length(modelTerms))
  for (i in 1:length(modelTerms)) {
    term <- modelTerms[[i]]
    termName <- paste(term$component, collapse = ":")
    names(isNuisance)[i] <- termName
    if (term$isNuisance)
      isNuisance[i] <- TRUE
  }
  return(isNuisance)
}

.bayesianLogisticRegGetPosteriorSummary <- function(bayesianLogisticRegContainer,
                                                    bayesianLogisticRegModel,
                                                    dataset, options, ready) {
  if (!ready || bayesianLogisticRegContainer$getError())
    return()

  if (!is.null(bayesianLogisticRegContainer[["postSumModel"]]))
    return(bayesianLogisticRegContainer[["postSumModel"]]$object)

  # required for the marginal posterior plots
  # done here such that the information in the plots and tables always matches
  # if a user selects the same options. (The method uses approximations and otherwise decimals are off)
  footnote <- NULL

  .setSeedJASP(options)
  coefBMA <- .bayesianLogisticRegOverwriteCoefBas(bayesianLogisticRegModel,
                                                  estimator = "BMA",
                                                  dataset = dataset,
                                                  options = options,
                                                  weights = bayesianLogisticRegModel[["weights"]])
  conf95BMA <- try(stats::confint(coefBMA, level = 0.95, nsim = options$numericalAccuracy))
  if (isTryError(conf95BMA)) {
    conf95BMA <- cbind(NA, NA, coefBMA$postmean)
    rownames(conf95BMA) <- coefBMA$namesx
    colnames(conf95BMA) <- c("2.5%", "97.5%", "beta")
    conf95BMA[is.nan(conf95BMA)] <- NA
    footnote <- gettext("Parameters estimates and/or credible intervals could not be calculated.")
  }

  # check if results of table and plots should match
  estimator <- switch(options$summaryType, best = "HPM", median = "MPM", "BMA")
  criVal <- options[["posteriorSummaryPlotCiLevel"]]
  if (estimator == "BMA" && isTRUE(all.equal(criVal, 0.95))) { # what we show under Marginal Posterior distributions
    coef <- coefBMA
    conf95 <- conf95BMA
  } else {
    .setSeedJASP(options)
    coef <- .bayesianLogisticRegOverwriteCoefBas(bayesianLogisticRegModel,
                                                 estimator = estimator,
                                                 dataset = dataset,
                                                 options = options,
                                                 weights = bayesianLogisticRegModel[["weights"]])
    conf95 <- stats::confint(coef, level = criVal, nsim = options$numericalAccuracy)
  }

  probne0 <- coef[["probne0"]]
  coefficients <- bayesianLogisticRegModel[["namesx"]]
  if (estimator == "HPM") {
    loopIdx <- which(abs(coef$postmean) > sqrt(.Machine$double.eps))
  } else if (estimator == "MPM") {
    loopIdx <- which(abs(coef$postmean) > sqrt(.Machine$double.eps))
    probne0 <- bayesianLogisticRegModel[["probne0"]]
  } else {
    loopIdx <- seq_along(coefficients)
  }

  postSumModel <- list(coef = coef, loopIdx = loopIdx, coefficients = coefficients, probne0 = probne0,
                           conf95 = conf95, coefBMA = coefBMA, conf95BMA = conf95BMA, footnote = footnote)

  bayesianLogisticRegContainer[["postSumModel"]] <- createJaspState(postSumModel)
  bayesianLogisticRegContainer[["postSumModel"]]$dependOn(c("summaryType", "posteriorSummaryPlotCiLevel", "numericalAccuracy",
                                               "seed", "setSeed"))

  return(postSumModel)
}

.bayesianLogisticRegOverwriteCoefBas <- function (bayesianLogisticRegModel, n.models, estimator = "BMA", dataset, options, weights = NULL) {
  # this function is an adaptation of BAS:::coef.bas
  # additional arguments:
  #
  # dataset
  # weights
  #
  # in addition, the formula object should be stored in the bas object.
  #
  # the original function evaluates things via eval(calls) constructions
  # JASP does not guarantree that this lookup structure works
  # so we need to modify this function.
  # this is only the case for the median model!

  # if there are future updates to the BAS package, this function can probably be removed
  # the code below is a small test for when an error happens.

  # data(UScrime, package = "MASS")
  # UScrime <- UScrime[, 1:5]
  # form <- M ~ So + Ed + Po1 + Po2
  # crime.bic =  BAS::bas.lm(
  #   formula = M ~ So + Ed + Po1 + Po2, # <-- toggle this one (works)
  #  # formula = form,                  # <-- and this one    (errors)
  #  data = UScrime,
  #  prior = "JZS",
  #  initprobs = c(1, 0.5, 0.5, 0.5, 0.5),
  #  renormalize = TRUE)
  # BAS:::coef.bas(crime.bic, estimator = "MPM") # <-- this function call will error

  # additionaly, the code previously failed (in JASP) for the correlation dataset (Big 5)
  # and selecting estimator = "MPM" (median model)
  # if neither of these errors occur in a future version then the original function can
  # probably be used again

  if (estimator == "MPM") {
    formulas <- .bayesianLogisticRegCreateFormula(options[["dependent"]], options[["modelTerms"]])
    modelFormula <- formulas[["modelFormula"]]
    nullFormula <- formulas[["nullFormula"]]

    nvar <- bayesianLogisticRegModel$n.vars - 1
    bestmodel <- (0:nvar)[bayesianLogisticRegModel$probne0 > 0.5]
    best <- 1
    models <- rep(0, nvar + 1)
    models[bestmodel + 1] <- 1
    if (sum(models) > 1) {
      bayesianLogisticRegModel <- BAS::bas.glm(formula = modelFormula,
                                               family  = binomial(link = "logit"),
                                               data    = dataset,
                                               weights = weights,
                                               n.models = 1,
                                               betaprior = bayesianLogisticRegModel$betaprior,
                                               modelprior = bayesianLogisticRegModel$modelprior,
                                               method     = toupper(options$samplingMethod),
                                               update = NULL,
                                               bestmodel = models,
                                               MCMC.iterations = NULL,
                                               renormalize  = TRUE,
                                               force.heredity  = TRUE,
                                               include.always = nullFormula)
    }
  }
  postprobs = bayesianLogisticRegModel$postprobs
  if (estimator == "MPM" | estimator == "HPM")
    n.models = 1
  if (missing(n.models))
    n.models = length(postprobs)
  topm = order(-postprobs)[1:n.models]
  postprobs = postprobs[topm]/sum(postprobs[topm])
  shrinkage = bayesianLogisticRegModel$shrinkage[topm]
  conditionalmeans = BAS:::list2matrix.bas(bayesianLogisticRegModel, "mle")[topm,
                                                               , drop = F]
  conditionalmeans[, -1] = sweep(conditionalmeans[, -1, drop = F],
                                 1, shrinkage, FUN = "*")
  postmean = as.vector(postprobs %*% conditionalmeans)
  conditionalsd = BAS:::list2matrix.bas(bayesianLogisticRegModel, "mle.se")[topm,
                                                               , drop = F]
  if (!(bayesianLogisticRegModel$betaprior$family == "AIC" || bayesianLogisticRegModel$betaprior$family == "BIC")) {
    conditionalsd[, -1] = sweep(conditionalsd[, -1, drop = F],
                                1, sqrt(shrinkage), FUN = "*")
  }
  postsd = sqrt(postprobs %*% conditionalsd^2 + postprobs %*%
                  ((sweep(conditionalmeans, 2, postmean, FUN = "-"))^2))
  postsd = as.vector(postsd)
  if (is.null(bayesianLogisticRegModel$df[topm])) {
    df = rep(bayesianLogisticRegModel$n, length(postprobs))
    if (bayesianLogisticRegModel$prior == "BIC" | bayesianLogisticRegModel$prior == "AIC") {
      df = df - bayesianLogisticRegModel$size
    }
    else {
      df = df - 1
    }
  }
  else df = bayesianLogisticRegModel$df[topm]
  out = list(postmean = postmean, postsd = postsd, probne0 = bayesianLogisticRegModel$probne0,
             conditionalmeans = conditionalmeans, conditionalsd = conditionalsd,
             namesx = bayesianLogisticRegModel$namesx, postprobs = postprobs, n.vars = bayesianLogisticRegModel$n.vars,
             n.models = n.models, df = df, estimator = estimator)
  class(out) = "coef.bas"
  return(out)
}

.bayesianLogisticRegComputePriorMarginalInclusionProbs <- function(bayesianLogisticRegModel) {
  # Calculate the prior inclusions probabilities for each predictor
  #
  # Args:
  #   model: bas object (including nuisanceTerms entry)
  #
  # Return:
  #   vector of inclusion probabilities (including intercept)

  allModels <- bayesianLogisticRegModel$which
  modelProbs <- bayesianLogisticRegModel$priorprobs
  nPreds <- length(bayesianLogisticRegModel$probne0)

  # model prior has been modified, recalculate the prior inclusion probs
  nModels <- length(allModels)
  priorProbs <- numeric(nPreds)

  for (i in 1:nModels) {

    idx <- allModels[[i]] + 1 # +1 to change 0 for intercept into a 1 so it can be used as an index
    priorProbs[idx] = priorProbs[idx] + modelProbs[i]

  }

  return(priorProbs)
}

.bayesianLogisticRegComputeInclusionBF <- function(bayesianLogisticRegModel) {
  nModels <- bayesianLogisticRegModel[["n.models"]]
  nPred <- length(bayesianLogisticRegModel[["probne0"]])

  # should this work on a log scale??
  # first row is numerator of the odds; second row is denominator
  priorOdds <- matrix(0, 2, nPred)
  posteriorOdds <- matrix(0, 2, nPred)
  for (i in seq_len(nModels)) {

    idxN <- bayesianLogisticRegModel[["which"]][[i]] + 1
    idxD <- (1:nPred)[-idxN]

    # increment numerators
    priorOdds[1, idxN] <- priorOdds[1, idxN] + bayesianLogisticRegModel[["priorprobs"]][i]
    posteriorOdds[1, idxN] <- posteriorOdds[1, idxN] + bayesianLogisticRegModel[["postprobs"]][i]

    # increment denominators
    priorOdds[2, idxD] <- priorOdds[2, idxD] + bayesianLogisticRegModel[["priorprobs"]][i]
    posteriorOdds[2, idxD] <- posteriorOdds[2, idxD] + bayesianLogisticRegModel[["postprobs"]][i]

  }

  priOdds <- priorOdds[1, ] / priorOdds[2, ]
  posOdds <- posteriorOdds[1, ] / posteriorOdds[2, ]
  BFinclusion <- posOdds / priOdds

  # nuisance terms and intercept are always included
  BFinclusion[-1][bayesianLogisticRegModel[["nuisanceTerms"]]] <- 1 # nuisance terms
  BFinclusion[1] <- 1 # intercept

  return(BFinclusion)
}

.bayesianLogisticRegReplaceInteractionUnicodeSymbol <- function(name) {
  # ggplot can't show the interaction symbol
  gsub("\u2009\u273b\u2009", " x ", name, fixed = TRUE)
}

.basregExportResiduals <- function(bayesianLogisticRegContainer, bayesianLogisticRegModel, dataset, options, ready) {

  if (!ready)
    return()

  userWantsResiduals   <- options[["residualsSavedToData"]]   && options[["residualsSavedToDataColumn"]]   != "" && is.null(bayesianLogisticRegContainer[["residualsSavedToDataColumn"]])
  userWantsResidualSds <- options[["residualSdsSavedToData"]] && options[["residualSdsSavedToDataColumn"]] != "" && is.null(bayesianLogisticRegContainer[["residualSdsSavedToDataColumn"]])

  if (!userWantsResiduals && !userWantsResidualSds)
    return()

  if (options[["summaryType"]] == "complex") {

    # find the most complex model
    mostComplexIdx <- which.max(lengths(bayesianLogisticRegModel[["which"]]))

    # we copy the bas object and pretend that the most complex model is the best model,
    # so that estimator "HPM" does exactly what we want
    bayesianLogisticRegModelTemp <- bayesianLogisticRegModel
    bayesianLogisticRegModelTemp[["postprobs"]][-mostComplexIdx] <- 0
    bayesianLogisticRegModelTemp[["postprobs"]][ mostComplexIdx] <- 1
    predictions <- predict(bayesianLogisticRegModelTemp, se.fit = userWantsResidualSds, estimator = "HPM")

  } else if (options[["summaryType"]] == "median") {

    # We do this for the same reason we need .basregOverwritecoefBas some weird lazy evaluation issues in R.
    # See also https://github.com/merliseclyde/BAS/issues/56, once that is fixed we can probably remove this
    weights <- NULL
    if (options$weights != "") {
      weightsVar <- options$weights
      weights <- dataset[[weightsVar]]
    }

    bayesianLogisticRegModelTemp <- bayesianLogisticRegModel
    bayesianLogisticRegModelTemp$call$formula <- formula(bayesianLogisticRegModel$terms)
    bayesianLogisticRegModelTemp$call$data    <- dataset
    bayesianLogisticRegModelTemp$call$weights <- weights

    predictions <- predict(bayesianLogisticRegModelTemp, se.fit = userWantsResidualSds, estimator = "MPM")

  } else {

    estimator <- switch(options[["summaryType"]], best = "HPM", median = "MPM", "BMA")
    predictions <- predict(bayesianLogisticRegModel, se.fit = userWantsResidualSds, estimator = estimator)

  }

  if (userWantsResiduals) {

    residuals <- c(bayesianLogisticRegModel[["Y"]] - predictions[["fit"]]) # c to drop attributes

    bayesianLogisticRegContainer[["residualsSavedToDataColumn"]] <- createJaspColumn(columnName = options[["residualsSavedToDataColumn"]])
    bayesianLogisticRegContainer[["residualsSavedToDataColumn"]]$dependOn(options = c("residualsSavedToDataColumn", "residualsSavedToData", "summaryType"))
    bayesianLogisticRegContainer[["residualsSavedToDataColumn"]]$setScale(residuals)

  }

  if (userWantsResidualSds) {

    residualsSds <- predictions[[if (options[["summaryType"]] == "averaged") "se.bma.pred" else "se.pred"]]

    bayesianLogisticRegContainer[["residualSdsSavedToDataColumn"]] <- createJaspColumn(columnName = options[["residualSdsSavedToDataColumn"]])
    bayesianLogisticRegContainer[["residualSdsSavedToDataColumn"]]$dependOn(options = c("residualSdsSavedToDataColumn", "residualSdsSavedToData", "summaryType"))
    bayesianLogisticRegContainer[["residualSdsSavedToDataColumn"]]$setScale(residualsSds)

  }
}
