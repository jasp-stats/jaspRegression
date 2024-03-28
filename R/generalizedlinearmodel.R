#
# Copyright (C) 2013-2018 University of Amsterdam
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

#still to do: multinomial, ordinal, negative binomial, quasi

GeneralizedLinearModelInternal <- function(jaspResults, dataset = NULL, options, ...) {
  if (options[["family"]] == "binomial") {
    ready <- (options[["dependent"]] != "" && options[["weights"]] != "")

  } else {
    ready <- options[["dependent"]] != ""
  }

  if (ready) {
    dataset <- .glmReadData(dataset, options)
    .glmCheckDataErrors(dataset, options)
  }

  #output tables
  .glmModelSummaryTable(jaspResults, dataset, options, ready, position = 1)
  .glmModelFitTable(    jaspResults, dataset, options, ready, position = 2)
  .glmEstimatesTable(   jaspResults, dataset, options, ready, position = 3)
  if (options$family == "other") return()

  #diagnostic tables and plots
  .glmDiagnostics(jaspResults, dataset, options, ready, position = 4)

  #estimated marginal means table and contrast analysis
  .glmEmm(jaspResults, dataset, options, ready, position = 5)

  return()
}

# Function to read data
.glmReadData <- function(dataset, options) {
  if (!is.null(dataset)) {
    return(dataset)
  }
  else {
    numericVars  <- unlist(c(options[["covariates"]], options[["weights"]], options[["offset"]]))
    numericVars  <- numericVars[numericVars != ""]
    factorVars   <- options[["factors"]]
    factorVars   <- factorVars[factorVars != ""]
    dependentVar <- options[["dependent"]]
    dependentVar <- dependentVar[dependentVar != ""]

    if (options[["family"]] %in% c("bernoulli", "other")) {
      return(.readDataSetToEnd(columns.as.numeric  = numericVars,
                               columns.as.factor   = c(factorVars, dependentVar),
                               exclude.na.listwise = c(numericVars, factorVars, dependentVar)))
    }
    else {
      return(.readDataSetToEnd(columns.as.numeric  = c(numericVars, dependentVar),
                               columns.as.factor   = factorVars,
                               exclude.na.listwise = c(numericVars, factorVars, dependentVar)))
    }
  }
}

# Function to check errors when reading data
.glmCheckDataErrors <- function(dataset, options){

  if (length(options[["factors"]]) == 0)
    nFactorParameters <- 0
  else
    nFactorParameters <- sum(apply(dataset[options[["factors"]]], 2, function(x) length(unique(x)))) - length(options[["factors"]])

  if (length(options[["covariates"]]) == 0) {
    nCovariateParameters <- 0
  }
  else {
    nCovariateParameters <- length(options[["covariates"]])
    .hasErrors(dataset,
               type = c("observations", "infinity", "variance", "varCovData"),
               all.target = options[["covariates"]],
               observations.amount  = "< 2",
               exitAnalysisIfErrors = TRUE)
  }

  nParameters <- options[["interceptTerm"]] + nFactorParameters + nCovariateParameters
  if (nrow(dataset) < nParameters)
    .quitAnalysis("The dataset contains fewer observations than the number of parameters (after excluding NAs/NaN/Inf).")

  if (options[["weights"]] != "")
    .hasErrors(dataset,
               type = "limits",
               limits.target = options[["weights"]],
               limits.min = 0,
               limits.max = Inf,
               exitAnalysisIfErrors = TRUE)

  if (options[["family"]] == "bernoulli") {

    if (length(levels(dataset[, options[["dependent"]]])) != 2)
      .quitAnalysis(gettext("The Bernoulli family requires the dependent variable to be a factor with 2 levels."))

  } else if (options[["family"]] == "binomial") {

    if (any(dataset[, options[["dependent"]]] < 0) || any(dataset[, options[["dependent"]]] > 1))
      .quitAnalysis(gettext("The Binomial family requires the dependent variable (i.e. proportion of successes) to be between 0 and 1 (inclusive)."))

    if (any(dataset[, options[["weights"]]] < 0) || any(!.is.wholenumber(dataset[, options[["weights"]]])))
      .quitAnalysis(gettext("The Binomial family requires the weights variable (i.e. total number of trials) to be an integer."))

  } else if (options[["family"]] %in% c("Gamma", "inverse.gaussian")) {

    if (any(dataset[, options[["dependent"]]] <= 0))
      .quitAnalysis(gettext("The Gamma family and the Inverse Gaussian family require the dependent variable to be positive."))

  } else if (options[["family"]] == "poisson") {

    if (any(dataset[, options[["dependent"]]] < 0 | any(!.is.wholenumber(dataset[, options[["dependent"]]]))))
      .quitAnalysis(gettext("The Poisson family requires the dependent variable to be an integer."))

  } else if (options[["family"]] == "gaussian") {

    if (options[["link"]] == "log" & any(dataset[[options[["dependent"]]]] <= 0))
      .quitAnalysis(gettextf("The Gaussian family with the log link requires the dependent variable to be positive."))

    if (!is.numeric(dataset[, options[["dependent"]]]))
      .quitAnalysis(gettextf("The Gaussian family requires the dependent variable to be a numerical variable."))
  } else if (options[["family"]] == "other") {
    if (options[["otherGlmModel"]] == "multinomialLogistic" && nlevels(dataset[[options[["dependent"]]]]) < 3) {
      .quitAnalysis(gettext("Multinomial logistic regression requires the dependent variable to be a factor with at least 3 levels."))
    } else if (options[["otherGlmModel"]] == "ordinalLogistic" && nlevels(dataset[[options[["dependent"]]]]) < 3) {
      .quitAnalysis(gettext("Ordinal logistic regression requires the dependent variable to be a factor with at least 3 levels."))
    } else if (options[["otherGlmModel"]] == "firthLogistic" && nlevels(dataset[[options[["dependent"]]]]) != 2) {
      .quitAnalysis(gettext("Firth logistic regression requires the dependent variable to be a factor with 2 levels."))
    }
  }
}

# Model Summary Table
.glmModelSummaryTable <- function(jaspResults, dataset, options, ready, position) {
  if (!is.null(jaspResults[["modelSummary"]])) {
    return()
  }

  if (!ready) {
    modelSummary <- createJaspTable(gettext("Model Summary"))
  }
  else {
    modelSummary <- createJaspTable(gettextf("Model Summary - %s", options[['dependent']]))
  }

  dependList <- c("dependent", "family", "link", "modelTerms", "interceptTerm", "weights", "offset", "otherGlmModel")
  modelSummary$dependOn(dependList)
  modelSummary$position <- position
  modelSummary$showSpecifiedColumnsOnly <- TRUE

  modelSummary$addColumnInfo(name = "mod", title = gettext("Model"),    type = "string")
  modelSummary$addColumnInfo(name = "dev", title = gettext("Deviance"), type = "number")
  modelSummary$addColumnInfo(name = "aic", title = gettext("AIC"),      type = "number")
  modelSummary$addColumnInfo(name = "bic", title = gettext("BIC"),      type = "number")
  modelSummary$addColumnInfo(name = "dof", title = gettext("df"),       type = "integer")
  modelSummary$addColumnInfo(name = "chi", title = "\u03A7\u00B2",      type = "number")
  modelSummary$addColumnInfo(name = "pvl", title = gettext("p"),        type = "pvalue")

  jaspResults[["modelSummary"]] <- modelSummary
  .glmModelSummaryTableFill(jaspResults, dataset, options, ready)

  return()
}

.glmModelSummaryTableFill <- function(jaspResults, dataset, options, ready) {
  if (ready) {
    # compute glm models
    glmModels <- .glmComputeModel(jaspResults, dataset, options)
    hasNuisance <- .hasNuisance(options)
    if (hasNuisance) {
      if ((options[["family"]] == "other") & (options[["otherGlmModel"]] %in% c("multinomialLogistic", "ordinalLogistic")))
        termsNullModel <- rownames(VGAM::coef(VGAM::summaryvglm(glmModels[["nullModel"]])))
      else if (options[["otherGlmModel"]] == "firthLogistic")
        termsNullModel <- names(coef((glmModels[["nullModel"]])))
      else
        termsNullModel <- rownames(coef(summary(glmModels[["nullModel"]])))
      nuisanceTerms <- jaspBase::gsubInteractionSymbol(termsNullModel[!grepl("(Intercept)", termsNullModel, fixed = TRUE)])
      message <- gettextf("Null model contains nuisance parameters: %s.",
                          paste(nuisanceTerms, collapse = ", "))
      jaspResults[["modelSummary"]]$addFootnote(message)
    }

    #log-likelihood ratio test to compare nested models (null vs full)
    if (options[["family"]] %in% c("bernoulli", "binomial", "poisson", "other")) {
      testType <- "Chisq"
      pvalName <- "Pr(>Chi)"
    }

    else {
      testType <- "F"
      pvalName <- "Pr(>F)"
    }

    if (options[["family"]] == "other") {
      if (options[["otherGlmModel"]] == "firthLogistic") {
        devNullModel <- glmModels[["nullModel"]][["loglik"]]["full"]*-2
        aicNullModel <- devNullModel + 2*glmModels[["nullModel"]][["df"]]
        bicNullModel <- devNullModel + log(nobs(glmModels[["nullModel"]]))*glmModels[["nullModel"]][["df"]]
        dofNullModel <- glmModels[["nullModel"]][["n"]] - glmModels[["nullModel"]][["df"]]

        devFullModel <- glmModels[["fullModel"]][["loglik"]]["full"]*-2
        aicFullModel <- devFullModel + 2*glmModels[["fullModel"]][["df"]]
        bicFullModel <- devFullModel + log(nobs(glmModels[["fullModel"]]))*glmModels[["fullModel"]][["df"]]
        dofFullModel <- glmModels[["fullModel"]][["n"]] - glmModels[["fullModel"]][["df"]]

        chiValue     <- devNullModel - devFullModel
        pValue       <- 1 - pchisq(chiValue, glmModels[["fullModel"]][["df"]])
      }
      else {
        devNullModel <- VGAM::deviance(glmModels[["nullModel"]])
        aicNullModel <- VGAM::AIC(glmModels[["nullModel"]])
        bicNullModel <- VGAM::BIC(glmModels[["nullModel"]])
        dofNullModel <- VGAM::df.residual(glmModels[["nullModel"]])

        devFullModel <- VGAM::deviance(glmModels[["fullModel"]])
        aicFullModel <- VGAM::AIC(glmModels[["fullModel"]])
        bicFullModel <- VGAM::BIC(glmModels[["fullModel"]])
        dofFullModel <- VGAM::df.residual(glmModels[["fullModel"]])

        anovaRes     <- VGAM::anova.vglm(glmModels[["nullModel"]], glmModels[["fullModel"]], type = 1)
        chiValue     <- devNullModel - devFullModel
        pValue       <- anovaRes[["Pr(>Chi)"]][[2]]
      }
    } else {
      devNullModel <- glmModels[["nullModel"]][["deviance"]]
      aicNullModel <- glmModels[["nullModel"]][["aic"]]
      bicNullModel <- BIC(glmModels[["nullModel"]])
      dofNullModel <- glmModels[["nullModel"]][["df.residual"]]

      devFullModel <- glmModels[["fullModel"]][["deviance"]]
      aicFullModel <- glmModels[["fullModel"]][["aic"]]
      bicFullModel <- BIC(glmModels[["fullModel"]])
      dofFullModel <- glmModels[["fullModel"]][["df.residual"]]

      anovaRes     <- anova(glmModels[["nullModel"]], glmModels[["fullModel"]],
                            test = testType)
      chiValue     <- anovaRes$Deviance[[2]]
      pValue       <- anovaRes[[pvalName]][[2]]
    }

    rows <- list(
      list(mod = "H\u2080",
           dev = devNullModel,
           aic = aicNullModel,
           bic = bicNullModel,
           dof = dofNullModel,
           chi = "",
           pvl = ""),
      list(mod = "H\u2081",
           dev = devFullModel,
           aic = aicFullModel,
           bic = bicFullModel,
           dof = dofFullModel,
           chi = chiValue,
           pvl = pValue)
    )
  } else {
    rows <- list(
      list(mod = "H\u2080"),
      list(mod = "H\u2081")
    )
  }
  jaspResults[["modelSummary"]]$addRows(rows)
}

# Model fit table
.glmModelFitTable <- function(jaspResults, dataset, options, ready, position) {
  if (!is.null(jaspResults[["modelFit"]]) || (!options[["devianceGoodnessOfFit"]] & !options[["pearsonGoodnessOfFit"]])) {
    return()
  }

  modelFitTable <- createJaspTable(gettext("Model Fit"))


  modelFitTable$dependOn(optionsFromObject   = jaspResults[["modelSummary"]],
                         options             = c("devianceGoodnessOfFit", "pearsonGoodnessOfFit"))

  modelFitTable$position <- position
  modelFitTable$showSpecifiedColumnsOnly <- TRUE

  modelFitTable$addColumnInfo(name = "gofType",   title = "",                    type = "string")
  modelFitTable$addColumnInfo(name = "gof",       title = gettext("Statistic"),  type = "number")
  modelFitTable$addColumnInfo(name = "dof",       title = gettext("df"),         type = "integer")
  modelFitTable$addColumnInfo(name = "pval",      title = gettext("p"),          type = "pvalue")

  jaspResults[["modelFitTable"]] <- modelFitTable
  .glmModelFitTableFill(jaspResults, dataset, options, ready)

  return()
}

.glmModelFitTableFill <- function(jaspResults, dataset, options, ready) {
  if (!ready)
    return()

  # compute glm models
  glmModels <- .glmComputeModel(jaspResults, dataset, options)
  modelObj  <- glmModels[["fullModel"]]

  if (options[["otherGlmModel"]] == "firthLogistic") {
    dev <- modelObj[["loglik"]]["full"]*-2
    pearsonResid <- (modelObj[['y']] - modelObj[["predict"]])/sqrt(modelObj[["predict"]]*(1-modelObj[["predict"]]))
    pearson <- sum(pearsonResid^2)
    dof <- modelObj[["n"]] - modelObj[["df"]]

  } else {

    if (options[["family"]] == "other")
      pearson <- sum(VGAM::residuals(modelObj, type = "pearson")^2)
    else
      pearson <- sum(residuals(modelObj, type = "pearson")^2)
    dev <- deviance(modelObj)
    dof <- df.residual(modelObj)
  }

  if (options[["devianceGoodnessOfFit"]]) {
    jaspResults[["modelFitTable"]]$addRows(
      list(gofType = "Deviance",
           gof     = dev,
           dof     = dof,
           pval    = pchisq(dev,
                            df=dof,
                            lower.tail=FALSE))
    )
  }

  if (options[["pearsonGoodnessOfFit"]]) {
    jaspResults[["modelFitTable"]]$addRows(
      list(gofType = "Pearson",
           gof     = pearson,
           dof     = dof,
           pval    = pchisq(pearson,
                            df=dof,
                            lower.tail=FALSE))
    )
  }
}

# GLM estimates table
.glmEstimatesTable <- function(jaspResults, dataset, options, ready, position) {
  if (!options[["coefficientEstimate"]] || !is.null(jaspResults[["estimatesTable"]]))
    return()

  estimatesTable <- createJaspTable(gettext("Coefficients"))
  estimatesTable$dependOn(optionsFromObject   = jaspResults[["modelSummary"]],
                          options             = c("coefficientEstimate", "coefficientCi", "coefficientCiLevel"))
  estimatesTable$position <- position
  estimatesTable$showSpecifiedColumnsOnly <- TRUE

  if (options[["family"]] %in% c("bernoulli", "binomial", "poisson", "other"))
    testStat <- "z"
  else
    testStat <- "t"

  if (options["otherGlmModel"] == "firthLogistic")
    testStat <- "\u03A7\u00B2"

  estimatesTable$addColumnInfo(name = "param",    title = "", type = "string")
  estimatesTable$addColumnInfo(name = "est",      title = gettext("Estimate"), type = "number")
  estimatesTable$addColumnInfo(name = "se",       title = gettext("Standard Error"), type = "number")
  estimatesTable$addColumnInfo(name = "testStat", title = gettext(testStat), type = "number")
  estimatesTable$addColumnInfo(name = "pval",     title = gettext("p"), type = "pvalue")

  if (options[["coefficientCi"]]) {
    ciPercentage <- options[["coefficientCiLevel"]] * 100
    if (floor(ciPercentage) == ciPercentage)
      ciPercentage <- as.integer(ciPercentage)
    ciTitle <- paste(ciPercentage, " % ", "Confidence Interval",sep = "")
    estimatesTable$addColumnInfo(name = "ciLow",    title = gettext("Lower Bound"), type = "number", overtitle = ciTitle)
    estimatesTable$addColumnInfo(name = "ciUpp",    title = gettext("Upper Bound"), type = "number", overtitle = ciTitle)
  }

  jaspResults[["estimatesTable"]] <- estimatesTable
  .glmEstimatesTableFill(jaspResults, dataset, options, ready)
}

.glmEstimatesTableFill <- function(jaspResults, dataset, options, ready) {
  if (!ready)
    return()
  # compute glm models
  glmModels <- .glmComputeModel(jaspResults, dataset, options)
  fullModel <- glmModels[["fullModel"]]

  if ((options[["family"]] == "other") & (options[["otherGlmModel"]] %in% c("multinomialLogistic", "ordinalLogistic")))
    modelSummary <- VGAM::coef(VGAM::summaryvglm(fullModel))

  if (options[["family"]] == "other" && options[["otherGlmModel"]] == "firthLogistic")
    modelSummary <- cbind(coef(fullModel), sqrt(diag(fullModel$var)), qchisq(1 - fullModel$prob, 1), fullModel$prob)

  if (options[["family"]] != "other")
    modelSummary <- coef(summary(fullModel))

  rowNames <- rownames(modelSummary)

  if (options[["coefficientCi"]]) {
    coefCiSummary <- confint(fullModel, level = options[["coefficientCiLevel"]])
    if (length(rowNames) == 1) coefCiSummary <- matrix(coefCiSummary, ncol = 2)
  } else {
    coefCiSummary <- matrix(nrow = length(rowNames),
                            ncol = 2,
                            data = rep(0, length(rowNames)*2))
  }

  paramDf <- data.frame(param = sapply(rowNames, jaspBase::gsubInteractionSymbol))
  colnames(modelSummary) <- c('est', 'se', 'testStat', 'pval')
  colnames(coefCiSummary) <- c('ciLow', 'ciUpp')
  estimatesTableData <- cbind(paramDf, modelSummary, coefCiSummary)
  jaspResults[["estimatesTable"]]$setData(estimatesTableData)

  if (options[["family"]] == "bernoulli" || (options[["family"]] == "other" && options[["otherGlmModel"]] == "firthLogistic")) {
    dv      <- options$dependent
    dvLevel <- levels(dataset[[dv]])[2]

    jaspResults[["estimatesTable"]]$addFootnote(gettextf("%1$s level '%2$s' coded as class 1.", dv, dvLevel))
  }

  if (options["family"] == "other" && options[["otherGlmModel"]] == "multinomialLogistic") {
    dv      <- options$dependent
    dvReferenceLevel <- tail(levels(dataset[[dv]]), 1)
    dvLevels <- paste(paste(seq(1, length(dv)), levels(dataset[[dv]]), sep = ":"), collapse = ", ")

    jaspResults[["estimatesTable"]]$addFootnote(gettextf("%1$s levels: %2$s. '%3$s' is the reference level.", dv, dvLevels, dvReferenceLevel))
  }

  if (options["family"] == "other" && options[["otherGlmModel"]] == "ordinalLogistic") {
    dv      <- options$dependent
    dvLevels <- paste(paste(seq(1, length(dv)), levels(dataset[[dv]]), sep = ":"), collapse = ", ")
    linearPredictors <- paste(VGAM::summaryvglm(fullModel)@misc$predictors.names, collapse = ", ")

    jaspResults[["estimatesTable"]]$addFootnote(gettextf("%1$s levels: %2$s. Linear predictors: %3$s.", dv, dvLevels, linearPredictors))
  }
}

# Diagnostics container
.glmDiagnostics <- function(jaspResults, dataset, options, ready, position) {

  if (!ready) return()

  diagnosticsContainer <- createJaspContainer(title = gettext("Diagnostics"))
  diagnosticsContainer$position <- position
  jaspResults[["diagnosticsContainer"]] <- diagnosticsContainer

  .glmPlotResVsFitted(jaspResults, dataset, options, ready, position = 1)

  .glmPlotResVsPredictor(jaspResults, dataset, options, ready, residType = "deviance", position = 2)
  .glmPlotResVsPredictor(jaspResults, dataset, options, ready, residType = "Pearson", position = 3)
  .glmPlotResVsPredictor(jaspResults, dataset, options, ready, residType = "quantile", position = 4)

  .glmPlotResQQ(jaspResults, dataset, options, ready, position = 5)

  .glmPlotResPartial(jaspResults, dataset, options, ready, position = 6)

  .glmPlotZVsEta(jaspResults, dataset, options, ready, position = 7)

  .glmOutlierTable(jaspResults, dataset, options, ready, position = 8, residType = "quantile")
  .glmOutlierTable(jaspResults, dataset, options, ready, position = 8, residType = "standardized deviance")
  .glmOutlierTable(jaspResults, dataset, options, ready, position = 8, residType = "studentized deviance")

  .glmInfluenceTable(jaspResults[["diagnosticsContainer"]], 
                     jaspResults[["glmModels"]][["object"]][["fullModel"]],
                     dataset, options, ready, position = 9)
  
  .regressionExportResiduals(jaspResults, 
                             jaspResults[["glmModels"]][["object"]][["fullModel"]],
                             dataset, options, ready)
  
  .glmMulticolliTable(jaspResults, dataset, options, ready, position = 10)

  return()
}

# Plots: Residuals vs. fitted
.glmPlotResVsFitted <- function(jaspResults, dataset, options, ready, position = 4) {

  plotNames <- c("devianceResidualVsFittedPlot", "pearsonResidualVsFittedPlot", "quantileResidualVsFittedPlot")
  if (!ready || !any(unlist(options[plotNames])))
    return()

  residNames <- c("deviance", "Pearson", "quantile")

  glmPlotResVsFittedContainer <- createJaspContainer(gettext("Residuals vs. Fitted Plots"))
  glmPlotResVsFittedContainer$dependOn(optionsFromObject = jaspResults[["modelSummary"]],
                                       options           = c(plotNames, "seed", "setSeed"))
  glmPlotResVsFittedContainer$position <- position
  jaspResults[["diagnosticsContainer"]][["glmPlotResVsFitted"]] <- glmPlotResVsFittedContainer


  if (!is.null(jaspResults[["glmModels"]])) {
    glmFullModel <- jaspResults[["glmModels"]][["object"]][["fullModel"]]
    for (i in 1:length(plotNames)) {
      if (options[[plotNames[[i]]]]) {
        .glmCreatePlotPlaceholder(glmPlotResVsFittedContainer,
                                  index = plotNames[[i]],
                                  title = gettextf("Standardized %1s residuals vs. fitted values", residNames[[i]]))

        .glmInsertPlot(glmPlotResVsFittedContainer[[plotNames[[i]]]],
                       .glmFillPlotResVsFitted,
                       residType = residNames[[i]],
                       model = glmFullModel,
                       family = options[["family"]],
                       options = options)
      }
    }
  }
  return()
}

.glmFillPlotResVsFitted <- function(residType, model, family, options) {

  # compute residuals and fitted values
  stdResid <- .glmStdResidCompute(model = model, residType = residType, options = options)
  fittedY  <- fitted(model)

  # decide on constant-information scale transformations of fitted values
  fittedY  <- .constInfoTransform(family, fittedY)
  xlabText <- .constInfoTransName(family)

  # breaks and limits for pretty plots
  xBreaks <- pretty(fittedY)
  xLimits <- range(xBreaks)

  yBreaks <- pretty(stdResid)
  yLimits <- range(yBreaks)

  # make plot
  thePlot <- ggplot2::ggplot(mapping = ggplot2::aes(x = x, y = y),
                             data = data.frame(y = stdResid,
                                               x = fittedY)) +
    jaspGraphs::geom_point() + #this is prettier: size = 4, shape = 1
    ggplot2::xlab(xlabText) +
    ggplot2::ylab(gettextf("Standardized %1s residual", residType)) +
    ggplot2::geom_smooth(se = FALSE,
                         size = 0.6,
                         method = "loess",
                         method.args = list(degree = 1, family = "symmetric")) +
    ggplot2::scale_y_continuous(breaks = yBreaks, limits = yLimits) +
    ggplot2::scale_x_continuous(breaks = xBreaks, limits = xLimits) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  return(thePlot)
}



# Plots: Residuals vs. predictor
.glmPlotResVsPredictor <- function(jaspResults, dataset, options, ready, residType, position) {
  if (!ready)
    return()

  plotType <- switch(residType,
                     "deviance" = "devianceResidualVsPredictorPlot",
                     "Pearson"  = "pearsonResidualVsPredictorPlot",
                     "quantile" = "quantileResidualVsPredictorPlot")

  if (!options[[plotType]])
    return()

  predictors <- c(options[["covariates"]], options[["factors"]])

  glmPlotResVsPredictorContainer <- createJaspContainer(gettextf("%1s Residuals vs. Predictor Plots", .capitalize(residType)))
  glmPlotResVsPredictorContainer$dependOn(optionsFromObject = jaspResults[["modelSummary"]],
                                          options           = c(plotType, "seed", "setSeed"))
  glmPlotResVsPredictorContainer$position <- position
  jaspResults[["diagnosticsContainer"]][[plotType]] <- glmPlotResVsPredictorContainer


  if (!is.null(jaspResults[["glmModels"]])) {
    glmFullModel <- jaspResults[["glmModels"]][["object"]][["fullModel"]]
    for (predictor in predictors) {
      .glmCreatePlotPlaceholder(glmPlotResVsPredictorContainer,
                                index = predictor,
                                title = gettextf("Standardized %1$s residuals vs. %2$s", residType, predictor))

      .glmInsertPlot(glmPlotResVsPredictorContainer[[predictor]],
                     .glmFillPlotResVsPredictor,
                     residType = residType,
                     predictor = predictor,
                     model = glmFullModel,
                     options = options)
    }
  }
  return()
}

.glmFillPlotResVsPredictor <- function(residType, predictor, model, options) {

  # compute residuals
  stdResid <- .glmStdResidCompute(model = model, residType = residType, options = options)
  # get predictor values
  if (predictor %in% options[["factors"]]) {
    predictorVec <- factor(model$data[[predictor]])
  } else {
    predictorVec <- model$data[[predictor]]
  }

  # make plot
  d <- data.frame(y = stdResid,
                  x = predictorVec)

  yBreaks <- pretty(stdResid) #note that the breaks and limits work for y axis too
  yLimits <- range(yBreaks)

  if (is.factor(predictorVec)) {
    thePlot <- ggplot2::ggplot(mapping = ggplot2::aes(x = x, y = y),
                               data = d) +
      ggplot2::geom_boxplot() +
      jaspGraphs::geom_point() +
      ggplot2::xlab(gettext(predictor)) +
      ggplot2::ylab(gettextf("Standardized %1s residual", residType)) +
      ggplot2::scale_y_continuous(breaks = yBreaks, limits = yLimits) +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw()
  } else {
    xBreaks <- pretty(predictorVec)
    xLimits <- range(xBreaks)
    thePlot <- ggplot2::ggplot(mapping = ggplot2::aes(x = x, y = y),
                               data = d) +
      jaspGraphs::geom_point() +
      ggplot2::geom_smooth(se = FALSE,
                           size = 0.6,
                           method = "loess",
                           method.args = list(degree = 1, family = "symmetric")) +
      ggplot2::xlab(gettext(predictor)) +
      ggplot2::ylab(gettextf("Standardized %1s residual", residType)) +
      ggplot2::scale_x_continuous(breaks = xBreaks, limits = xLimits) +
      ggplot2::scale_y_continuous(breaks = yBreaks, limits = yLimits) +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw()
  }
  return(thePlot)
}



# Plots: Residuals Q-Q
.glmPlotResQQ <- function(jaspResults, dataset, options, ready, position) {

  plotNames <- c("devianceResidualQqPlot", "pearsonResidualQqPlot", "quantileResidualQqPlot")
  if (!ready || !any(unlist(options[plotNames])))
    return()

  residNames <- c("deviance", "Pearson", "quantile")

  glmPlotResQQContainer <- createJaspContainer(gettext("Normal Q-Q Plots: Standardized Residuals"))
  glmPlotResQQContainer$dependOn(optionsFromObject = jaspResults[["modelSummary"]],
                                 options           = c(plotNames, "seed", "setSeed"))
  glmPlotResQQContainer$position <- position
  jaspResults[["diagnosticsContainer"]][["glmPlotResQQ"]] <- glmPlotResQQContainer


  if (!is.null(jaspResults[["glmModels"]])) {
    glmFullModel <- jaspResults[["glmModels"]][["object"]][["fullModel"]]
    for (i in 1:length(plotNames)) {
      if (options[[plotNames[[i]]]]) {
        .glmCreatePlotPlaceholder(glmPlotResQQContainer,
                                  index = plotNames[[i]],
                                  title = gettextf("Normal Q-Q plot: Standardized %1s residuals", residNames[[i]]))

        .glmInsertPlot(glmPlotResQQContainer[[plotNames[[i]]]],
                       .glmFillPlotResQQ,
                       residType = residNames[[i]],
                       model = glmFullModel,
                       family = options[["family"]])
      }
    }
  }
  return()
}

.glmFillPlotResQQ <- function(residType, model, family) {

  # compute residuals
  stdResid <- .glmStdResidCompute(model = model, residType = residType, options = options)

  thePlot <- jaspGraphs::plotQQnorm(stdResid, ablineColor = "blue")

  return(thePlot)
}


# Plots: Partial residuals
.glmPlotResPartial <- function(jaspResults, dataset, options, ready, position) {
  if (!ready)
    return()

  if (!options[["partialResidualPlot"]])
    return()

  predictors <- c(options[["covariates"]], options[["factors"]])

  glmPlotResPartialContainer <- createJaspContainer(gettext("Partial Residual Plots"))
  glmPlotResPartialContainer$dependOn(optionsFromObject = jaspResults[["modelSummary"]],
                                      options           = "partialResidualPlot")
  glmPlotResPartialContainer$position <- position
  jaspResults[["diagnosticsContainer"]][["partialResidualPlot"]] <- glmPlotResPartialContainer


  if (!is.null(jaspResults[["glmModels"]])) {
    glmFullModel <- jaspResults[["glmModels"]][["object"]][["fullModel"]]
    for (predictor in predictors) {
      .glmCreatePlotPlaceholder(glmPlotResPartialContainer,
                                index = predictor,
                                title = gettextf("Partial residual plot for %1s", predictor))

      .glmInsertPlot(glmPlotResPartialContainer[[predictor]],
                     .glmFillPlotResPartial,
                     predictor = predictor,
                     model = glmFullModel,
                     options = options)
    }
  }
  return()
}

.glmFillPlotResPartial <- function(predictor, model, options) {

  # compute residuals
  partResidDf <- as.data.frame(resid(model, type = "partial"))
  partResid   <- partResidDf[[predictor]]

  yBreaks <- pretty(partResid)
  yLimits <- range(yBreaks)

  # get original predictor values
  if (predictor %in% options[["factors"]]) {
    predictorVec <- factor(model$data[[predictor]])
  } else {
    predictorVec <- model$data[[predictor]]
  }

  # make plot
  d <- data.frame(y = partResid,
                  x = predictorVec)
  if (is.factor(predictorVec)) {
    thePlot <- ggplot2::ggplot(mapping = ggplot2::aes(x = x, y = y),
                               data = d) +
      ggplot2::geom_boxplot() +
      jaspGraphs::geom_point() +
      ggplot2::xlab(gettext(predictor)) +
      ggplot2::ylab(gettextf("Partial residual for %1s", predictor)) +
      ggplot2::scale_y_continuous(breaks = yBreaks, limits = yLimits) +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw()
  } else {
    xBreaks <- pretty(predictorVec)
    xLimits <- range(xBreaks)
    thePlot <- ggplot2::ggplot(mapping = ggplot2::aes(x = x, y = y),
                               data = d) +
      jaspGraphs::geom_point() +
      ggplot2::geom_smooth(se = FALSE,
                           size = 0.6,
                           method = "loess",
                           method.args = list(degree = 1, family = "symmetric")) +
      ggplot2::xlab(gettext(predictor)) +
      ggplot2::ylab(gettextf("Partial residual for %1s", predictor)) +
      ggplot2::scale_x_continuous(breaks = xBreaks, limits = xLimits) +
      ggplot2::scale_y_continuous(breaks = yBreaks, limits = yLimits) +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw()
  }
  return(thePlot)
}



# Plot: Working responses vs. linear predictor
.glmPlotZVsEta <- function(jaspResults, dataset, options, ready, position) {
  if (!ready)
    return()

  if (!options[["workingResponseVsLinearPredictorPlot"]])
    return()

  glmPlotZVsEtaContainer <- createJaspContainer(gettext("Plot: Working responses vs. linear predictor"))
  glmPlotZVsEtaContainer$dependOn(optionsFromObject = jaspResults[["modelSummary"]],
                                  options           = "workingResponseVsLinearPredictorPlot")
  glmPlotZVsEtaContainer$position <- position
  jaspResults[["diagnosticsContainer"]][["glmPlotZVsEta"]] <- glmPlotZVsEtaContainer

  if (!is.null(jaspResults[["glmModels"]])) {
    glmFullModel <- jaspResults[["glmModels"]][["object"]][["fullModel"]]
    .glmCreatePlotPlaceholder(glmPlotZVsEtaContainer,
                              index = "glmPlotZVsEta",
                              title = gettext("Plot: Working responses vs. linear predictor"))

    .glmInsertPlot(glmPlotZVsEtaContainer[["glmPlotZVsEta"]],
                   .glmFillPlotZVsEta,
                   model = glmFullModel,
                   options = options)
  }
  return()
}

.glmFillPlotZVsEta <- function(model, options) {

  # compute linear predictor eta and working responses z
  eta <- model[["linear.predictors"]]
  z <- resid(model, type="working") + eta

  # make plot
  xBreaks <- pretty(z)
  xLimits <- range(xBreaks)
  yBreaks <- pretty(eta)
  yLimits <- range(yBreaks)

  thePlot <- ggplot2::ggplot(mapping = ggplot2::aes(x = x,
                                                    y = y),
                             data = data.frame(x = z,
                                               y = eta)) +
    jaspGraphs::geom_point() +
    ggplot2::geom_smooth(se = FALSE,
                         size = 0.6,
                         method = "loess",
                         method.args = list(degree = 1, family = "symmetric")) +
    ggplot2::xlab(gettext("Working responses, z")) +
    ggplot2::ylab(expression(paste("Linear predictor, ", hat(eta)))) +
    ggplot2::scale_x_continuous(breaks = xBreaks, limits = xLimits) +
    ggplot2::scale_y_continuous(breaks = yBreaks, limits = yLimits) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  return(thePlot)
}



# Table: GLM outliers
.glmOutlierTable <- function(jaspResults, dataset, options, ready, position, residType) {

  optionName <- switch(residType,
                       "quantile"              = "quantileResidualOutlierTable",
                       "standardized deviance" = "standardizedResidualOutlierTable",
                       "studentized deviance"  = "studentizedResidualOutlierTable")

  optionTopN <- switch(residType,
                       "quantile"              = "quantileResidualOutlierTableTopN",
                       "standardized deviance" = "standardizedResidualOutlierTableTopN",
                       "studentized deviance"  = "studentizedResidualOutlierTableTopN")

  if (!options[[optionName]] || !ready)
    return()

  if (is.null(jaspResults[["diagnosticsContainer"]][["outlierTables"]])) {
    glmOutlierTablesContainer <- createJaspContainer(gettext("Outliers Tables"))
    glmOutlierTablesContainer$dependOn(optionsFromObject = jaspResults[["modelSummary"]],
                                       options           = c("seed", "setSeed"))
    glmOutlierTablesContainer$position <- position
    jaspResults[["diagnosticsContainer"]][["outlierTables"]]     <- glmOutlierTablesContainer
  }

  outlierTable <- createJaspTable(gettextf("Table: Top n outliers based on %1s residuals", residType))
  outlierTable$dependOn(optionsFromObject   = jaspResults[["modelSummary"]],
                        options             = c(optionName, optionTopN, "seed", "setSeed"))
  outlierTable$showSpecifiedColumnsOnly <- TRUE

  outlierTable$addColumnInfo(name = "caseN",      title = gettext("Case Number"), type = "integer")
  outlierTable$addColumnInfo(name = "residScore", title = gettext("Residual"),    type = "number")

  jaspResults[["diagnosticsContainer"]][["outlierTables"]][[optionName]] <- outlierTable
  topN <- options[[optionTopN]]
  .glmOutlierTableFill(jaspResults, dataset, options, ready, residType, optionName, topN)
}

.glmOutlierTableFill <- function(jaspResults, dataset, options, ready, residType, optionName, topN) {
  if (!ready)
    return()

  if (!is.null(jaspResults[["glmModels"]])) {
    glmFullModel <- jaspResults[["glmModels"]][["object"]][["fullModel"]]
    if (residType == "quantile")
      jaspBase::.setSeedJASP(options)
    residVec       <- switch(residType,
                             "quantile" =  statmod::qresid(glmFullModel),
                             "standardized deviance" = rstandard(glmFullModel),
                             "studentized deviance"  = rstudent(glmFullModel))
    residDf <- data.frame(caseN       = seq.int(length(residVec)),
                          residScore  = residVec)
    residRankedDf <- residDf[order(abs(residVec), decreasing = TRUE), ]

    for (i in 1:topN) {
      jaspResults[["diagnosticsContainer"]][["outlierTables"]][[optionName]]$addRows(
        list(caseN = residRankedDf[i, "caseN"],
             residScore = residRankedDf[i, "residScore"])
      )
    }
  }
}


# Table: Influential cases
.glmInfluenceTable <- function(jaspResults, model, dataset, options, ready, position, linRegAnalysis = FALSE) {

  tableOptionsOn <- c(options[["dfbetas"]],
                      options[["dffits"]],
                      options[["covarianceRatio"]],
                      options[["leverage"]],
                      options[["mahalanobis"]])

  if (!ready | !options[["residualCasewiseDiagnostic"]])
    return()


  tableOptions <- c("dfbetas", "dffits", "covarianceRatio", "leverage", "mahalanobis")
  tableOptionsClicked <- tableOptions[tableOptionsOn]
  tableOptionsClicked <- c("cooksDistance", tableOptionsClicked)

  if (is.null(jaspResults[["influenceTable"]])) {
    influenceTable <- createJaspTable(gettext("Table: Influential Cases"))
    influenceTable$dependOn(optionsFromObject   = jaspResults[["modelSummary"]],
                            options             = tableOptions)
    influenceTable$dependOn(c("residualCasewiseDiagnostic", "residualCasewiseDiagnosticType",
                             "residualCasewiseDiagnosticZThreshold", "residualCasewiseDiagnosticCooksDistanceThreshold"))
    influenceTable$position <- position
    influenceTable$showSpecifiedColumnsOnly <- TRUE
    jaspResults[["influenceTable"]] <- influenceTable
  }

  tableOptionToColName <- function(x) {
    switch(x,
           "dfbetas"  = "DFBETAS",
           "dffits"   = "DFFITS",
           "covarianceRatio" = "Covariance Ratio",
           "cooksDistance"   = "Cook's Distance",
           "leverage" = "Leverage",
           "mahalanobis" = "Mahalanobis")
  }

  if (is.null(model)) {
    for (option in tableOptionsClicked) {
      colTitle    <- tableOptionToColName(option)
      influenceTable$addColumnInfo(name = option, title = gettext(colTitle), type = "number")
    }
  } else {
    
    colNameList  <- c()
    influenceTable$addColumnInfo(name = "caseN", title = "Case Number", type = "integer")
    influenceTable$addColumnInfo(name = "stdResidual", title = gettext("Std. Residual"),   type = "number", format = "dp:3")
    influenceTable$addColumnInfo(name = "dependent",   title = options$dependent,          type = "number")
    influenceTable$addColumnInfo(name = "predicted",   title = gettext("Predicted Value"), type = "number")
    influenceTable$addColumnInfo(name = "residual", title = gettext("Residual"),   type = "number", format = "dp:3")
    
    alwaysPresent <- c("caseN", "stdResidual", "dependent", "predicted", "residual")
    for (option in tableOptionsClicked) {
      if (option == "dfbetas") {
        predictors <- names(model$coefficients)
        for (predictor in predictors) {
          dfbetasName  <- gettextf("DFBETAS_%1s", predictor)
          colNameList <- c(colNameList, dfbetasName)
          if (predictor == "(Intercept)")
            dfbetasTitle <- gettext("DFBETAS:Intercept")
          else
            dfbetasTitle <- gettextf("DFBETAS:%1s", gsub(":", "*", predictor))
          influenceTable$addColumnInfo(name = dfbetasName, title = dfbetasTitle, type = "number")
        }
      } else {
        colNameList <- c(colNameList, option)
        colTitle    <- tableOptionToColName(option)
        influenceTable$addColumnInfo(name = option, title = gettext(colTitle), type = "number")
      }
    }
    .glmInfluenceTableFill(influenceTable, dataset, options, ready, 
                           model = model, 
                           influenceMeasures = tableOptionsClicked, 
                           colNames = c(colNameList, alwaysPresent))
  }
}

.glmInfluenceTableFill <- function(influenceTable, dataset, options, ready, model, influenceMeasures, colNames) {

  influenceRes <- influence.measures(model)
  nDFBETAS     <- length(names(model$coefficients))

  optionToColInd <- function(x, nDFBETAS) {
    switch(x,
           "dfbetas"  = 1:nDFBETAS,
           "dffits"   = (nDFBETAS+1),
           "covarianceRatio" = (nDFBETAS+2),
           "cooksDistance"   = (nDFBETAS+3),
           "leverage" = (nDFBETAS+4))}

  colInd <- c()
  for (measure in influenceMeasures) {
    colInd <- c(colInd, optionToColInd(measure, nDFBETAS))
  }

  influenceResData <- as.data.frame(influenceRes[["infmat"]][, colInd])
  colnames(influenceResData)[1:length(colInd)] <- colNames[1:length(colInd)]
  
  influenceResData[["caseN"]] <- seq.int(nrow(influenceResData))
  influenceResData[["stdResidual"]] <- rstandard(model)
  influenceResData[["dependent"]] <- model.frame(model)[[options$dependent]]
  influenceResData[["predicted"]] <- model$fitted.values
  influenceResData[["residual"]] <- model$residual
# browser()
  modelMatrix <- as.data.frame(model.matrix(model))
  modelMatrix <- modelMatrix[colnames(modelMatrix) != "(Intercept)"]
  influenceResData[["mahalanobis"]] <- mahalanobis(modelMatrix, center = colMeans(modelMatrix), cov = cov(modelMatrix))
  
  if (options$residualCasewiseDiagnosticType == "cooksDistance")
    index <- which(abs(influenceResData[["cooksDistance"]]) > options$residualCasewiseDiagnosticCooksDistanceThreshold)
  else if (options$residualCasewiseDiagnosticType == "outliersOutside")
    index <- which(abs(influenceResData[["stdResidual"]]) > options$residualCasewiseDiagnosticZThreshold)
  else # all
    index <- seq.int(nrow(influenceResData))

  # funky statement to ensure a df even if only 1 row
  influenceResSig       <- subset(influenceRes[["is.inf"]], 1:nrow(influenceResData) %in% index, select = colInd)
  colnames(influenceResSig) <- colNames[1:length(colInd)]
  
  influenceResData <- influenceResData[index, ]

  if (length(index) == 0)
    influenceTable$addFootnote(gettext("No influential cases found."))
  else {
    influenceTable$setData(influenceResData)
    # if any other metrix show influence, add footnotes:
    if (sum(influenceResSig) > 0) {
      for (thisCol in colnames(influenceResSig)) {
        if (sum(influenceResSig[, thisCol]) > 0) 
          influenceTable$addFootnote(
            gettext("Potentially influential case, according to the selected influence measure."), 
            colNames = thisCol,
            rowNames = rownames(influenceResData)[influenceResSig[, thisCol]],
            symbol = "*"
          )
      }
    }
  }
}

# Table: Multicollinearity
.glmMulticolliTable <- function(jaspResults, dataset, options, ready, position) {

  tableOptionsOn <- c(options[["tolerance"]],
                      options[["vif"]])

  if (!ready | !any(tableOptionsOn))
    return()

  if (length(c(options[["covariates"]], options[["factors"]])) == 1)
    .quitAnalysis("Multicollinearity analysis requires at least two predictors.")

  if (is.null(jaspResults[["diagnosticsContainer"]][["multicolliTable"]])) {
    multicolliTable <- createJaspTable(gettext("Multicollinearity Diagnostics"))
    multicolliTable$dependOn(optionsFromObject   = jaspResults[["modelSummary"]],
                             options             = c("tolerance", "vif"))
    multicolliTable$position <- position
    multicolliTable$showSpecifiedColumnsOnly <- TRUE
    jaspResults[["diagnosticsContainer"]][["multicolliTable"]] <- multicolliTable
  }


  jaspResults[["diagnosticsContainer"]][["multicolliTable"]]$addColumnInfo(name = "var", title = "", type = "string")

  if (is.null(jaspResults[["glmModels"]]))
    return()

  if (options[["tolerance"]])
    jaspResults[["diagnosticsContainer"]][["multicolliTable"]]$addColumnInfo(name = "tolerance", title = gettext("Tolerance"), type = "number")

  if (options[["vif"]])
    jaspResults[["diagnosticsContainer"]][["multicolliTable"]]$addColumnInfo(name = "VIF", title = gettext("VIF"), type = "number")

  glmFullModel <- jaspResults[["glmModels"]][["object"]][["fullModel"]]
  .glmMulticolliTableFill(jaspResults, dataset, options, ready, glmObj = glmFullModel)

}

.glmMulticolliTableFill <- function(jaspResults, dataset, options, ready, glmObj) {
  vif_obj       <- .vif.default(glmObj)

  if (is.matrix(vif_obj)) {
    var_names     <- rownames(vif_obj)
    n_var         <- length(var_names)
    vif_vec       <- vif_obj[,1]
    tolerance_vec <- 1/vif_vec
  }
  else {
    var_names     <- names(vif_obj)
    n_var         <- length(var_names)
    vif_vec       <- vif_obj
    tolerance_vec <- 1/vif_vec
  }

  for (i in 1:n_var) {
    jaspResults[["diagnosticsContainer"]][["multicolliTable"]]$addRows(list(var       = var_names[[i]],
                                                                            tolerance = tolerance_vec[[i]],
                                                                            VIF       = vif_vec[[i]]))
  }
}




.glmEmm <- function(jaspResults, dataset, options, ready, position) {

  if (!ready) return()

  emmContainer <- createJaspContainer(title = gettext("Estimated Marginal Means and Contrast Analysis"))
  emmContainer$position <- position
  jaspResults[["emmContainer"]] <- emmContainer

  .glmMarginalMeansTable(jaspResults, dataset, options, ready, position = 1)
  .glmContrastsTable(jaspResults, dataset, options, ready, position = 2)

  return()
}


# Estimated marginal means
.glmMarginalMeansTable <- function(jaspResults, dataset, options, ready, position) {

  if (!ready | (length(options[["marginalMeansVars"]]) == 0))
    return()

  if (!is.null(jaspResults[["emmContainer"]][["emmResults"]]))
    return()

  if (is.null(jaspResults[["glmModels"]]))
    return()

  glmFullModel <- jaspResults[["glmModels"]][["object"]][["fullModel"]]

  # deal with continuous predictors
  at <- NULL
  for (var in unlist(options[["marginalMeansVars"]])) {
    if (var %in% options[["covariates"]]) {
      at[[var]] <- c(mean(dataset[, var], na.rm = TRUE) + c(-1, 0, 1) * options[["marginalMeansSd"]] * sd(dataset[, var], na.rm = TRUE))
    }
  }

  # compute the results
  emm <- emmeans::emmeans(
    object  = glmFullModel,
    specs   = unlist(options[["marginalMeansVars"]]),
    at      = at,
    options = list(level  = options[["marginalMeansCiWidth"]]),
    type    = if (options[["marginalMeansResponse"]]) "response"
  )

  emmTable  <- as.data.frame(emm)
  if (options[["marginalMeansComparison"]])
    emmTest <- as.data.frame(emmeans::test(emm, null = options[["marginalMeansComparisonWith"]]))

  emmSummary <- createJaspTable(title = gettext("Estimated Marginal Means"))
  emmResults <- createJaspState()

  emmSummary$position <- position

  emmDependencies <- c("marginalMeansVars",
                       "marginalMeansCi",
                       "marginalMeansCiWidth",
                       "marginalMeansSd",
                       "marginalMeansComparison",
                       "marginalMeansComparisonWith",
                       "marginalMeansResponse")

  emmSummary$dependOn(optionsFromObject = jaspResults[["modelSummary"]],
                      options           = emmDependencies)
  emmResults$dependOn(optionsFromObject = jaspResults[["modelSummary"]],
                      options           = emmDependencies)


  for (variable in unlist(options[["marginalMeansVars"]])) {
    if (variable %in% options[["covariates"]])
      emmSummary$addColumnInfo(name = variable, title = variable, type = "number")
    else
      emmSummary$addColumnInfo(name = variable, title = variable, type = "string")

  }

  emmSummary$addColumnInfo(name = "estimate", title = gettext("Estimate"), type = "number")
  emmSummary$addColumnInfo(name = "se",       title = gettext("SE"),       type = "number")

  if (options[["marginalMeansCi"]]) {
    emmSummary$addColumnInfo(name = "lowerCI",  title = gettext("Lower"),    type = "number", overtitle = gettextf("%s%% CI", 100 * options[["marginalMeansCiWidth"]]))
    emmSummary$addColumnInfo(name = "upperCI",  title = gettext("Upper"),    type = "number", overtitle = gettextf("%s%% CI", 100 * options[["marginalMeansCiWidth"]]))
  }

  if (options[["marginalMeansComparison"]]) {
    emmSummary$addColumnInfo(name = "stat",   title = ifelse(colnames(emmTest)[ncol(emmTest) - 1] == "t.ratio", gettext("t"), gettext("z")), type = "number")
    emmSummary$addColumnInfo(name = "pval",   title = gettext("p"),         type = "pvalue")
    emmSummary$addFootnote(.emmMessageTestNull(options[["marginalMeansComparisonWith"]]), colNames = "pval")
  }

  jaspResults[["emmContainer"]][["emmSummary"]] <- emmSummary

  for (i in 1:nrow(emmTable)) {
    tempRow <- list()

    if (options[["marginalMeansContrast"]])
      tempRow$Level <- i


    for (variable in unlist(options[["marginalMeansVars"]])) {

      if (variable %in% options[["covariates"]])
        tempRow[variable] <- emmTable[i, variable]
      else
        tempRow[variable] <- as.character(emmTable[i, variable])

    }

    # the estimate is before SE (names change for GLM)
    tempRow$estimate <- emmTable[i, grep("SE", colnames(emmTable)) - 1]
    tempRow$se       <- emmTable[i, "SE"]

    if (options[["marginalMeansComparison"]]) {
      tempRow$stat <- emmTest[i, grep("ratio", colnames(emmTest))]
      tempRow$pval <- emmTest[i, "p.value"]
    }

    if (options[["marginalMeansCi"]]) {
      tempRow$lowerCI  <- emmTable[i, ncol(emmTable) - 1]
      tempRow$upperCI  <- emmTable[i, ncol(emmTable)]
    }



    emmSummary$addRows(tempRow)
  }

  if (length(emm@misc$avgd.over) != 0)
    emmSummary$addFootnote(.emmMessageAveragedOver(emm@misc$avgd.over))

  # add warning message
  emmSummary$addFootnote(ifelse(options[["marginalMeansResponse"]],
                                gettext("Results are on the response scale."),
                                gettext("Results are not on the response scale and might be misleading.")))

  object <- list(
    emm        = emm,
    emmTable   = emmTable
  )

  emmResults$object <- object
  jaspResults[["emmContainer"]][["emmResults"]] <- emmResults

  return()
}

# Contrast analysis
.glmContrastsTable <- function(jaspResults, dataset, options, ready, position) {


  if (!ready | (length(options[["marginalMeansVars"]]) == 0) | !options[["marginalMeansContrast"]])
    return()

  if (!is.null(jaspResults[["emmContainer"]][["contrastsTable"]]))
    return()

  if (is.null(jaspResults[["emmContainer"]][["emmResults"]]))
    return()

  emm       <- jaspResults[["emmContainer"]][["emmResults"]]$object$emm
  emmTable  <- jaspResults[["emmContainer"]][["emmResults"]]$object$emmTable


  emmContrastSummary <- createJaspTable(title = gettext("Contrasts"))

  emmContrastSummary$position <- position

  emmContrastSummary$dependOn(optionsFromObject = jaspResults[["emmContainer"]][["emmResults"]],
                              options           = c("contrasts", "marginalMeansPAdjustment", "marginalMeansContrast"))


  emmContrastSummary$addColumnInfo(name = "contrast", title = "",                  type = "string")
  emmContrastSummary$addColumnInfo(name = "estimate", title = gettext("Estimate"), type = "number")
  emmContrastSummary$addColumnInfo(name = "se",       title = gettext("SE"),       type = "number")
  emmContrastSummary$addColumnInfo(name = "df",       title = gettext("df"),       type = "number")
  emmContrastSummary$addColumnInfo(name = "stat",     title = gettext("z"),        type = "number")
  emmContrastSummary$addColumnInfo(name = "pval",     title = gettext("p"),        type = "pvalue")

  # Columns have been specified, show to user
  jaspResults[["emmContainer"]][["contrastsTable"]] <- emmContrastSummary

  selectedContrasts        <- options[["contrasts"]]
  selectedPvalueAdjustment <- options[["marginalMeansPAdjustment"]]

  selectedResponse         <- options[["marginalMeansResponse"]]


  contrs <- list()
  for (cont in selectedContrasts[sapply(selectedContrasts, function(x) x$isContrast)]) {

    if (all(cont$values == 0))
      next

    contrs[[cont$name]] <- unname(sapply(cont$values, function(x) eval(parse(text = x))))

  }

  if (length(contrs) == 0)
    return()

  # take care of the scale
  if (selectedResponse) {
    emmContrast <- try(
      as.data.frame(
        emmeans::contrast(
          emmeans::regrid(emm),
          contrs,
          adjust = selectedPvalueAdjustment
          )
        )
      )
    } else {
    emmContrast <- try(
      as.data.frame(
        emmeans::contrast(
          emm,
          contrs,
          adjust = selectedPvalueAdjustment
          )
        )
      )
  }

  if (jaspBase::isTryError(emmContrast)) {
    emmContrastSummary$setError(emmContrast)
    return()
  }

  # fix the title name if there is a t-stats
  if (colnames(emmContrast)[5] == "t.ratio")
    emmContrastSummary$setColumnTitle("stat", gettext("t"))

  tempEstName <- colnames(emmContrast)[ncol(emmContrast) - 4]

  if (tempEstName == "odds.ratio")
    emmContrastSummary$setColumnTitle("estimate", gettext("Odds Ratio"))
  else if (tempEstName == "ratio")
    emmContrastSummary$setColumnTitle("estimate", gettext("Ratio"))
  else if (tempEstName == "estimate")
    emmContrastSummary$setColumnTitle("estimate", gettext("Estimate"))
  else
    emmContrastSummary$setColumnTitle("estimate", tempEstName)


  for (i in 1:nrow(emmContrast)) {

    tempRow <- list(
      contrast =  names(contrs)[i],
      estimate =  emmContrast[i, ncol(emmContrast) - 4],
      se       =  emmContrast[i, "SE"],
      df       =  emmContrast[i, "df"],
      stat     =  emmContrast[i, ncol(emmContrast) - 1],
      pval     =  emmContrast[i, "p.value"]
    )

    emmContrastSummary$addFootnote(.messagePvalAdjustment(selectedPvalueAdjustment), colNames = "pval")

    if (!selectedResponse)
      emmContrastSummary$addFootnote(gettext("Results are on the response scale."))
    else
      emmContrastSummary$addFootnote(gettext("Results are not on the response scale and might be misleading."))

    emmContrastSummary$addRows(tempRow)
  }

  return()
}

