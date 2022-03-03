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

RegressionLinear <- function(jaspResults, dataset = NULL, options) {
  ready <- options$dependent != "" && (length(unlist(options$modelTerms)) > 0 || options$includeConstant)

  if (ready) {
    dataset <- .linregReadDataset(dataset, options)
    .linregCheckErrors(dataset, options)
  }

  modelContainer  <- .linregGetModelContainer(jaspResults, position = 1)
  model           <- .linregCalcModel(modelContainer, dataset, options, ready)

  # these output elements show statistics of all the lm fits
  if (is.null(modelContainer[["summaryTable"]]))
    .linregCreateSummaryTable(modelContainer, model, options, position = 1)

  if (options$modelFit && is.null(modelContainer[["anovaTable"]]))
    .linregCreateAnovaTable(modelContainer, model, options, position = 2)

  if (options$regressionCoefficientsEstimates && is.null(modelContainer[["coeffTable"]]))
    .linregCreateCoefficientsTable(modelContainer, model, dataset, options, position = 3)

  if (options$regressionCoefficientsBootstrapping && is.null(modelContainer[["bootstrapCoeffTable"]]))
    .linregCreateBootstrapCoefficientsTable(modelContainer, model, dataset, options, position = 4)

  if (options$partAndPartialCorrelations && is.null(modelContainer[["partialCorTable"]]))
    .linregCreatePartialCorrelationsTable(modelContainer, model, dataset, options, position = 6)

  if (options$regressionCoefficientsCovarianceMatrix && is.null(modelContainer[["coeffCovMatrixTable"]]))
    .linregCreateCoefficientsCovarianceMatrixTable(modelContainer, model, options, position = 7)

  if (options$collinearityDiagnostics && is.null(modelContainer[["collinearityTable"]]))
    .linregCreateCollinearityDiagnosticsTable(modelContainer, model, options, position = 8)

  # these output elements show statistics of the "final model" (lm fit with all predictors in enter method and last lm fit in stepping methods)
  finalModel <- model[[length(model)]]

  if (options$residualsCasewiseDiagnostics && is.null(modelContainer[["casewiseTable"]]))
    .linregCreateCasewiseDiagnosticsTable(modelContainer, finalModel, options, position = 9)

  if (options$residualsStatistics && is.null(modelContainer[["residualsTable"]]))
    .linregCreateResidualsTable(modelContainer, finalModel, options, position = 10)

  if (options$plotResidualsDependent && is.null(modelContainer[["residualsVsDepPlot"]]))
     .linregCreateResidualsVsDependentPlot(modelContainer, finalModel, options, position = 11)

  if (options$plotResidualsCovariates && is.null(modelContainer[["residualsVsCovContainer"]]))
    .linregCreateResidualsVsCovariatesPlots(modelContainer, finalModel, dataset, options, position = 12)

  if (options$plotResidualsPredicted && is.null(modelContainer[["residualsVsPredPlot"]]))
    .linregCreateResidualsVsPredictedPlot(modelContainer, finalModel, options, position = 13)

  if (options$plotResidualsHistogram && is.null(modelContainer[["residualsVsHistPlot"]]))
    .linregCreateResidualsVsHistogramPlot(modelContainer, finalModel, options, position = 14)

  if (options$plotResidualsQQ && is.null(modelContainer[["residualsQQPlot"]]))
    .linregCreateResidualsQQPlot(modelContainer, finalModel, options, position = 15)

  if (options$plotsMarginal && is.null(modelContainer[["marginalPlotsContainer"]]))
    .linregCreateMarginalPlots(modelContainer, finalModel, dataset, options, position = 17)

  # these output elements do not use statistics of a pre-calculated lm fit
  if (options$plotsPartialRegression && is.null(modelContainer[["partialPlotContainer"]]))
    .linregCreatePartialPlots(modelContainer, dataset, options, position = 16)
  if (options$descriptives && is.null(modelContainer[["descriptivesTable"]]))
    .linregCreateDescriptivesTable(modelContainer, dataset, options, position = 5)
}
#TODO: capture crashes with many interactions between factors!
.linregReadDataset <- function(dataset, options) {
  if (!is.null(dataset))
    return(dataset)

  numericVariables  <- c(options$dependent, unlist(options$covariates), options$wlsWeights)
  numericVariables  <- numericVariables[numericVariables != ""]
  factors           <- unlist(options$factors)
  dataset           <- .readDataSetToEnd(columns.as.factor = factors, columns.as.numeric = numericVariables, exclude.na.listwise = c(factors, numericVariables))

  return(dataset)
}

.linregCheckErrors <- function(dataset, options) {
  stepwiseProcedureChecks <- NULL
  if (options$method %in% c("backward", "forward", "stepwise")) {
    stepwiseProcedureChecks <- list(

      checkIfContainsInteractions = function() {
        hasInteractions <- FALSE

        for (i in seq_along(options$modelTerms))
          if (length(options$modelTerms[[i]]$components) > 1)
            hasInteractions <- TRUE

        if (hasInteractions)
          return(gettext("Stepwise procedures are not supported for models containing interaction terms"))

        if (any(vapply(dataset, is.factor, logical(1L))))
          return(gettext("Stepwise procedures are not supported for models containing factors"))
      },

      checkIfPEntryIsValid = function() {
        if (options$steppingMethodCriteriaType == "usePValue" && options$steppingMethodCriteriaPEntry > options$steppingMethodCriteriaPRemoval)
          return(gettext("Error in Stepping Method Criteria: Entry p-value needs to be smaller than removal p-value"))
      },

      checkIfFEntryIsValid = function() {
        if (options$steppingMethodCriteriaType == "useFValue" && options$steppingMethodCriteriaFEntry < options$steppingMethodCriteriaFRemoval)
          return(gettext("Error in Stepping Method Criteria: Entry F-value needs to be larger than removal F-value"))
      }

    )
  }

  defaultTarget <- c(options$dependent, unlist(options$covariates))
  .hasErrors(dataset, type = c("infinity", "variance", "observations", "modelInteractions", "varCovData"),
             custom = stepwiseProcedureChecks,
             custom.target = defaultTarget,

             observations.amount = "< 2",
             observations.target = defaultTarget,

             modelInteractions.modelTerms = options$modelTerms,
             modelInteractions.target = defaultTarget,

             varCovData.target = unlist(options$covariates),
             varCovData.corFun = stats::cov,

             exitAnalysisIfErrors = TRUE)

  if (options$wlsWeights != "") {
    .hasErrors(dataset, type = c("infinity", "limits", "observations"),
               all.target = options$wlsWeights, limits.min = 0, observations.amount = "< 2",
               exitAnalysisIfErrors = TRUE)

    covwt <- function(...) return(stats::cov.wt(..., wt = dataset[[.v(options[["wlsWeights"]])]])$cov)
    .hasErrors(dataset[, -which(colnames(dataset) %in% c(.v(options$wlsWeights)))],  type = "varCovData", varCovData.corFun = covwt,
               exitAnalysisIfErrors = TRUE)
  }
}

.linregGetModelContainer <- function(jaspResults, position) {
  if (is.null(jaspResults[["modelContainer"]])) {
    modelContainer <- createJaspContainer()
    modelContainer$dependOn(c("dependent", "method", "covariates", "factors", "wlsWeights", "modelTerms", "steppingMethodCriteriaType",
                              "steppingMethodCriteriaPEntry", "steppingMethodCriteriaPRemoval", "steppingMethodCriteriaFEntry", "steppingMethodCriteriaFRemoval",
                              "includeConstant", "missingValues"))
    modelContainer$position <- position
    jaspResults[["modelContainer"]] <- modelContainer
  }
  return(jaspResults[["modelContainer"]])
}

.linregCreateSummaryTable <- function(modelContainer, model, options, position) {
  if(options[['dependent']] == "")
    summaryTable <- createJaspTable(gettext("Model Summary"))
  else
    summaryTable <- createJaspTable(gettextf("Model Summary - %s", options[['dependent']]))

  summaryTable$dependOn(c("residualsDurbinWatson", "rSquaredChange"))
  summaryTable$position <- position
  summaryTable$showSpecifiedColumnsOnly <- TRUE

  summaryTable$addColumnInfo(name = "model",  title = gettext("Model"),                    type = "string")
  summaryTable$addColumnInfo(name = "R",      title = gettext("R"),                        type = "number", format = "dp:3")
  summaryTable$addColumnInfo(name = "R2",     title = gettextf("R%s", "\u00B2"),           type = "number", format = "dp:3")
  summaryTable$addColumnInfo(name = "adjR2",  title = gettextf("Adjusted R%s", "\u00B2"),  type = "number", format = "dp:3")
  summaryTable$addColumnInfo(name = "RMSE",   title = gettext("RMSE"),                     type = "number")

  if (options$rSquaredChange) {
    summaryTable$addColumnInfo(name = "R2c",  title = gettextf("R%s Change", "\u00B2"), type = "number", format = "dp:3")
    summaryTable$addColumnInfo(name = "Fc",   title = gettext("F Change"),              type = "number")
    summaryTable$addColumnInfo(name = "df1",  title = gettext("df1"),                   type = "integer")
    summaryTable$addColumnInfo(name = "df2",  title = gettext("df2"),                   type = "integer")
    summaryTable$addColumnInfo(name = "p",    title = gettext("p"),                     type = "pvalue")
  }

  if (options$residualsDurbinWatson) {
    summaryTable$addColumnInfo(name = "DW_ac",  title = gettext("Autocorrelation"),  type = "number", overtitle = gettext("Durbin-Watson"))
    summaryTable$addColumnInfo(name = "DW",     title = gettext("Statistic"),        type = "number", overtitle = gettext("Durbin-Watson"))

    if (options$wlsWeights == "")
      summaryTable$addColumnInfo(name = "DW_p", title = gettext("p"), type = "pvalue", overtitle = "Durbin-Watson")
    else
      summaryTable$addFootnote(message = gettext("p-value for Durbin-Watson test is unavailable for weighted regression."))
  }

  .linregAddPredictorsInNullFootnote(summaryTable, options$modelTerms)

  if (!is.null(model)) {
    if (length(model) == 1 && length(model[[1]]$predictors) == 0 && !options$includeConstant)
      summaryTable$addFootnote(gettext("No covariate could be entered in the model"))
    else
      .linregFillSummaryTable(summaryTable, model)
  }

  modelContainer[["summaryTable"]] <- summaryTable
}

.linregFillSummaryTable <- function(summaryTable, model) {
  for (i in seq_along(model)) {
    lmSummary     <- model[[i]][["summary"]]
    rSquareChange <- model[[i]][["rSquareChange"]]
    durbinWatson  <- model[[i]][["durbinWatson"]]

    summaryTable$addRows(list(
      model = model[[i]]$title,
      R     = as.numeric(sqrt(lmSummary$r.squared)),
      R2    = as.numeric(lmSummary$r.squared),
      adjR2 = as.numeric(lmSummary$adj.r.squared),
      RMSE  = as.numeric(lmSummary$sigma),
      R2c   = rSquareChange$R2c,
      Fc    = rSquareChange$Fc,
      df1   = rSquareChange$df1,
      df2   = rSquareChange$df2,
      p     = rSquareChange$p,
      DW_ac = durbinWatson$r,
      DW    = durbinWatson$dw,
      DW_p  = durbinWatson$p
    ))
  }
}

.linregCreateAnovaTable <- function(modelContainer, model, options, position) {
  anovaTable <- createJaspTable(gettext("ANOVA"))
  anovaTable$dependOn(c("modelFit", "VovkSellkeMPR"))
  anovaTable$position <- position
  anovaTable$showSpecifiedColumnsOnly <- TRUE

  anovaTable$addColumnInfo(name = "model", title = gettext("Model"),          type = "string", combine = TRUE)
  anovaTable$addColumnInfo(name = "cases", title = "",                        type = "string")
  anovaTable$addColumnInfo(name = "SS",    title = gettext("Sum of Squares"), type = "number")
  anovaTable$addColumnInfo(name = "df",    title = gettext("df"),             type = "integer")
  anovaTable$addColumnInfo(name = "MS",    title = gettext("Mean Square"),    type = "number")
  anovaTable$addColumnInfo(name = "F",     title = gettext("F"),              type = "number")
  anovaTable$addColumnInfo(name = "p",     title = gettext("p"),              type = "pvalue")

  .linregAddPredictorsInNullFootnote(anovaTable, options$modelTerms)
  .linregAddVovkSellke(anovaTable, options$VovkSellkeMPR)

  if (!is.null(model)) {
    .linregAddInterceptNotShownFootnote(anovaTable, model, options)
    .linregFillAnovaTable(anovaTable, model, options)
  }

  modelContainer[["anovaTable"]] <- anovaTable
}

.linregFillAnovaTable <- function(anovaTable, model, options) {
  rowTypes <- list(Regression = gettext("Regression"), Residual = gettext("Residual"), Total = gettext("Total"))

  indicesOfModelsWithPredictors <- .linregGetIndicesOfModelsWithPredictors(model, options)
  for (i in indicesOfModelsWithPredictors) {
    isNewGroup  <- i > 1
    anovaRes    <- .linregGetAnova(model[[i]]$fit, model[[i]]$predictors)

    for (rowType in names(rowTypes)) {
      anovaTable$addRows(c(anovaRes[[rowType]], list(.isNewGroup = isNewGroup, model = model[[i]]$title, cases = rowTypes[[rowType]])))
      isNewGroup <- FALSE
    }
  }
}

.linregCreateCoefficientsTable <- function(modelContainer, model, dataset, options, position) {
  coeffTable <- createJaspTable("Coefficients")
  coeffTable$dependOn(c("regressionCoefficientsEstimates", "regressionCoefficientsConfidenceIntervals", "regressionCoefficientsConfidenceIntervalsInterval",
                        "collinearityDiagnostics", "VovkSellkeMPR"))
  coeffTable$position <- position
  coeffTable$showSpecifiedColumnsOnly <- TRUE

  coeffTable$addColumnInfo(name = "model",        title = gettext("Model"),          type = "string", combine = TRUE)
  coeffTable$addColumnInfo(name = "name",         title = "",                        type = "string")
  coeffTable$addColumnInfo(name = "unstandCoeff", title = gettext("Unstandardized"), type = "number")
  coeffTable$addColumnInfo(name = "SE",           title = gettext("Standard Error"), type = "number")
  coeffTable$addColumnInfo(name = "standCoeff",   title = gettext("Standardized"),   type = "number")
  coeffTable$addColumnInfo(name = "t",            title = gettext("t"),              type = "number")
  coeffTable$addColumnInfo(name = "p",            title = gettext("p"),              type = "pvalue")

  .linregAddVovkSellke(coeffTable, options$VovkSellkeMPR)

  if (options$regressionCoefficientsConfidenceIntervals) {
    overtitle <- gettextf("%.0f%% CI", 100 * options$regressionCoefficientsConfidenceIntervalsInterval)
    coeffTable$addColumnInfo(name = "lower", title = gettext("Lower"), type = "number", overtitle = overtitle)
    coeffTable$addColumnInfo(name = "upper", title = gettext("Upper"), type = "number", overtitle = overtitle)
  }

  if (options$collinearityDiagnostics) {
    overtitle <- gettext("Collinearity Statistics")
    coeffTable$addColumnInfo(name = "tolerance",  title = gettext("Tolerance"),  type = "number", format = "dp:3", overtitle = overtitle)
    coeffTable$addColumnInfo(name = "VIF",        title = gettext("VIF"),        type = "number",                  overtitle = overtitle)

  }

  if (!is.null(model)) {
    .linregAddFootnotePredictorsNeverIncluded(coeffTable, model, options)
    .linregAddFootnoteFactors(coeffTable, options[["factors"]], options[["collinearityDiagnostics"]])
    .linregFillCoefficientsTable(coeffTable, model, dataset, options)
  }

  modelContainer[["coeffTable"]] <- coeffTable
}

.linregAddFootnotePredictorsNeverIncluded <- function(coeffTable, model, options) {
  if (options$method %in% c("forward", "stepwise")) {
    includedPredictors <- unlist(lapply(model, "[[", "predictors"))
    neverIncludedPredictors <- setdiff(unlist(options$covariates), .unv(includedPredictors))

    if (length(neverIncludedPredictors) > 0) {
      message <- sprintf(ngettext(length(neverIncludedPredictors),
                                  "The following covariate was considered but not included: %s.",
                                  "The following covariates were considered but not included: %s."),
                         paste(neverIncludedPredictors, collapse=", "))
      coeffTable$addFootnote(message)
    }
  }
}

.linregAddFootnoteFactors <- function(table, factors, collinearityDiagnostics = FALSE) {
  if (length(factors) > 0L) {
    if (collinearityDiagnostics) {
      colNames <- c("standCoeff", "VIF", "tolerance")
      message <- gettext("Standardized coefficients and collinearity statistics can only be computed for continuous predictors.")
    } else {
      colNames <- "standCoeff"
      message <- gettext("Standardized coefficients can only be computed for continuous predictors.")
    }
    table$addFootnote(colNames = colNames, message = message)
  }
}

.linregAddFootnoteAliasedCoefficients <- function(table, rows) {
  table$addFootnote(gettext("Missing coefficients are undefined because of singularities. Check the data for anything out of order!"))
}

.linregFillCoefficientsTable <- function(coeffTable, model, dataset, options) {
  for (i in seq_along(model)) {
    isNewGroup <- i > 1

    temp <- .linregGetCoefficients(model[[i]]$fit, model[[i]]$predictors, dataset, options)
    coefficients <- temp[["coefficients"]]
    footnoteRows <- temp[["footnote"]]

    for (j in seq_along(coefficients)) {
      coeffTable$addRows(c(coefficients[[j]], list(.isNewGroup = isNewGroup, model = model[[i]]$title)))
      isNewGroup <- FALSE
    }

    # TODO: should this be done only after joining all obtained footnotes?
    if (!is.null(footnoteRows))
      .linregAddFootnoteAliasedCoefficients(coeffTable, footnoteRows)

  }
}

.linregCreateBootstrapCoefficientsTable <- function(modelContainer, model, dataset, options, position) {
  bootstrapCoeffTable <- createJaspTable(gettext("Bootstrap Coefficients"))
  bootstrapCoeffTable$dependOn(c("regressionCoefficientsEstimates", "regressionCoefficientsConfidenceIntervals", "regressionCoefficientsConfidenceIntervalsInterval",
                                 "regressionCoefficientsBootstrapping", "regressionCoefficientsBootstrappingReplicates"))
  bootstrapCoeffTable$position <- position
  bootstrapCoeffTable$showSpecifiedColumnsOnly <- TRUE

  bootstrapCoeffTable$addColumnInfo(name = "model",        title = gettext("Model"),          type = "string", combine = TRUE)
  bootstrapCoeffTable$addColumnInfo(name = "name",         title = "",                        type = "string")
  bootstrapCoeffTable$addColumnInfo(name = "unstandCoeff", title = gettext("Unstandardized"), type = "number")
  bootstrapCoeffTable$addColumnInfo(name = "bias",         title = gettext("Bias"),           type = "number")
  bootstrapCoeffTable$addColumnInfo(name = "SE",           title = gettext("Standard Error"), type = "number")

  if (options$regressionCoefficientsConfidenceIntervals) {
    overtitle <- gettextf("%s%% bca\u002A CI", 100 * options$regressionCoefficientsConfidenceIntervalsInterval)
    bootstrapCoeffTable$addColumnInfo(name = "lower", title = gettext("Lower"), type = "number", overtitle = overtitle)
    bootstrapCoeffTable$addColumnInfo(name = "upper", title = gettext("Upper"), type = "number", overtitle = overtitle)
  }

  bootstrapCoeffTable$addFootnote(gettextf("Bootstrapping based on %i replicates.", options[['regressionCoefficientsBootstrappingReplicates']]))
  bootstrapCoeffTable$addFootnote(gettext("Coefficient estimate is based on the median of the bootstrap distribution."))
  bootstrapCoeffTable$addFootnote(gettext("Bias corrected accelerated"), symbol = "\u002A")

  modelContainer[["bootstrapCoeffTable"]] <- bootstrapCoeffTable
  if (!is.null(model))
    .linregFillBootstrapCoefficientsTable(bootstrapCoeffTable, modelContainer, model, dataset, options)

}

.linregFillBootstrapCoefficientsTable <- function(bootstrapCoeffTable, modelContainer, model, dataset, options) {

  metaCols <- .linregGetTitlesAndIsNewGroups(model)

  if (is.null(modelContainer[["bootstrapCoefficients"]])) {
    startProgressbar(options$regressionCoefficientsBootstrappingReplicates * length(model))
    bootstrapCoeffTable$setData(metaCols)

    anyMissingValues <- FALSE
    coefficients <- NULL

    for (i in seq_along(model)) {
      data <- .linregGetBootstrapCoefficients(model[[i]]$fit, dataset, options)
      anyMissingValues <- anyNA(data[-1L])
      coefficients <- rbind(coefficients, data)
      bootstrapCoeffTable$setData(.linregCombineMetaWithData(metaCols, coefficients))
    }

    if (anyMissingValues)
      bootstrapCoeffTable$addFootnote(gettext("Some bootstrap coefficients or confidence intervals could not be computed."))

    modelContainer[["bootstrapCoefficients"]] <- createJaspState(coefficients)
    modelContainer[["bootstrapCoefficients"]]$dependOn(c("regressionCoefficientsBootstrappingReplicates", "regressionCoefficientsConfidenceIntervalsInterval"))
  } else {
    bootstrapCoeffTable$setData(.linregCombineMetaWithData(metaCols, modelContainer[["bootstrapCoefficients"]]$object))
  }
}

.linregGetTitlesAndIsNewGroups <- function(model, includeConstant) {
  isNewGroup  <- logical(0)
  titles      <- character(0)
  for (i in seq_along(model)) {
    if (is.null(model[[i]]$fit))
      next

    numPredictors <- length(stats::coef(model[[i]]$fit))

    isNewGroupCurrent <- i > 1
    if (numPredictors > 1)
      isNewGroupCurrent <- c(isNewGroupCurrent, logical(numPredictors - 1))

    isNewGroup  <- c(isNewGroup, isNewGroupCurrent)
    titles      <- c(titles, rep(model[[i]]$title, numPredictors))
  }

  return(data.frame(.isNewGroup = isNewGroup, model = titles))
}

.linregCombineMetaWithData <- function(meta, data) { # this can go once we can add cells to a jaspTable
  if (!is.data.frame(meta) || !is.data.frame(data))
    stop(gettext("expecting both arguments to be data.frames"))

  filler            <- matrix(NA, ncol(data), nrow = nrow(meta)-nrow(data))
  colnames(filler)  <- names(data)
  data              <- rbind(data, filler)

  return(cbind(meta, data))
}

.linregCreatePartialCorrelationsTable <- function(modelContainer, model, dataset, options, position) {
  partPartialTable <- createJaspTable(gettext("Part And Partial Correlations"))
  partPartialTable$dependOn("partAndPartialCorrelations")
  partPartialTable$position <- position

  partPartialTable$addColumnInfo(name = "model",   title = gettext("Model"),   type = "string", combine = TRUE)
  partPartialTable$addColumnInfo(name = "name",    title = "",                 type = "string")
  partPartialTable$addColumnInfo(name = "partial", title = gettext("Partial"), type = "number", format = "dp:3")
  partPartialTable$addColumnInfo(name = "part",    title = gettext("Part"),    type = "number", format = "dp:3")

  if (!is.null(model)) {
    .linregAddInterceptNotShownFootnote(partPartialTable, model, options)
    .linregFillPartialCorrelationsTable(partPartialTable, model, dataset, options)
  }

  modelContainer[["partialCorTable"]] <- partPartialTable
}

.linregFillPartialCorrelationsTable <- function(partPartialTable, model, dataset, options) {
  indicesOfModelsWithPredictors <- .linregGetIndicesOfModelsWithPredictors(model, options)
  for (i in indicesOfModelsWithPredictors) {
    isNewGroup <- i > 1

    cors <- .linregGetPartAndPartialCorrelation(model[[i]]$fit, model[[i]]$predictors, dataset, options)

    for (j in seq_along(cors)) {
      partPartialTable$addRows(c(cors[[j]], list(.isNewGroup = isNewGroup, model = model[[i]]$title)))
      isNewGroup <- FALSE
    }
  }
}

.linregCreateCoefficientsCovarianceMatrixTable <- function(modelContainer, model, options, position) {
  covMatTable <- createJaspTable(gettext("Coefficients Covariance Matrix"))
  covMatTable$dependOn("regressionCoefficientsCovarianceMatrix")
  covMatTable$position <- position

  covMatTable$addColumnInfo(name = "model", title = gettext("Model"), type = "string", combine = TRUE)
  covMatTable$addColumnInfo(name = "name",  title = "",               type = "string")

  if (!is.null(model)) {
    .linregAddPredictorsAsColumns(covMatTable, model, includeIntercept = FALSE)
    .linregAddInterceptNotShownFootnote(covMatTable, model, options)
    .linregFillCoefficientsCovarianceMatrixTable(covMatTable, model, options)
  }

  modelContainer[["coeffCovMatrixTable"]] <- covMatTable
}

.linregFillCoefficientsCovarianceMatrixTable <- function(covMatTable, model, options) {
  rawNames    <- setdiff(.linregGetParameterNames(model), "(Intercept)") # names used by R
  prettyNames <- setdiff(.linregMakePrettyNames(model),   "(Intercept)") # names shown in JASP

  indicesOfModelsWithPredictors <- .linregGetIndicesOfModelsWithPredictors(model, options)
  for (i in indicesOfModelsWithPredictors) {
    isNewGroup <- i > 1

    covData <- .linregGetCovarianceMatrix(model[[i]]$fit, rawNames, prettyNames)
    if (nrow(covData) > 0) {
      covData <- cbind(covData, .isNewGroup = c(isNewGroup, rep(F, nrow(covData) - 1)), model = model[[i]]$title)
      covMatTable$addRows(covData)
    }
  }
}

.linregCreateCollinearityDiagnosticsTable <- function(modelContainer, model, options, position) {
  collDiagTable <- createJaspTable(gettext("Collinearity Diagnostics"))
  collDiagTable$dependOn("collinearityDiagnostics")
  collDiagTable$position <- position

  collDiagTable$addColumnInfo(name = "model",      title = gettext("Model"),           type = "string", combine = TRUE)
  collDiagTable$addColumnInfo(name = "dimension",  title = gettext("Dimension"),       type = "integer")
  collDiagTable$addColumnInfo(name = "eigenvalue", title = gettext("Eigenvalue"),      type = "number")
  collDiagTable$addColumnInfo(name = "condIndex",  title = gettext("Condition Index"), type = "number")

  if (!is.null(model)) {
    .linregAddPredictorsAsColumns(collDiagTable, model, options[["includeConstant"]], overtitle = gettext("Variance Proportions"), format = "dp:3")
    .linregAddInterceptNotShownFootnote(collDiagTable, model, options)
    .linregFillCollinearityDiagnosticsTable(collDiagTable, model, options)
  }

  modelContainer[["collinearityTable"]] <- collDiagTable
}

.linregFillCollinearityDiagnosticsTable <- function(collDiagTable, model, options) {
  columns <- .linregGetPredictorColumnNames(model, options$modelTerms)
  indicesOfModelsWithPredictors <- .linregGetIndicesOfModelsWithPredictors(model, options)
  for (i in indicesOfModelsWithPredictors) {
    isNewGroup <- i > 1

    collDiagData <- try(.linregGetCollinearityDiagnostics(model[[i]]$fit, columns, options$includeConstant))
    if (jaspBase::isTryError(collDiagData)) {

      collDiagData <- cbind(.isNewGroup = isNewGroup, model = model[[i]]$title)
      collDiagTable$addFootnote(gettext("Some collinearity diagnostics could not be computed. This is expected if the model contains singularities."))

    } else {
      collDiagData <- cbind(collDiagData, .isNewGroup = c(isNewGroup, rep(F, nrow(collDiagData) - 1)), model = model[[i]]$title)
    }
    collDiagTable$addRows(collDiagData)
  }
}

.linregCreateCasewiseDiagnosticsTable <- function(modelContainer, finalModel, options, position) {
  caseDiagTable <- createJaspTable(gettext("Casewise Diagnostics"))
  caseDiagTable$dependOn(c("residualsCasewiseDiagnostics", "residualsCasewiseDiagnosticsType",
                           "residualsCasewiseDiagnosticsOutliersOutside", "residualsCasewiseDiagnosticsCooksDistance"))
  caseDiagTable$position <- position

  caseDiagTable$addColumnInfo(name = "caseNumber",  title = gettext("Case Number"),     type = "integer")
  caseDiagTable$addColumnInfo(name = "stdResidual", title = gettext("Std. Residual"),   type = "number", format = "dp:3")
  caseDiagTable$addColumnInfo(name = "dependent",   title = options$dependent,          type = "number")
  caseDiagTable$addColumnInfo(name = "predicted",   title = gettext("Predicted Value"), type = "number")
  caseDiagTable$addColumnInfo(name = "residual",    title = gettext("Residual"),        type = "number")
  caseDiagTable$addColumnInfo(name = "cooksD",      title = gettext("Cook's Distance"), type = "number", format = "dp:3")

  if (!is.null(finalModel))
    caseDiagTable$setData(.linregGetCasewiseDiagnostics(finalModel$fit, options)) #TODO: maybe add footnote to casewise diagnostics if there are no cases to show?

  modelContainer[["casewiseTable"]] <- caseDiagTable
}

.linregCreateResidualsTable <- function(modelContainer, finalModel, options, position) {
  residualsTable <- createJaspTable(gettext("Residuals Statistics"))
  residualsTable$dependOn("residualsStatistics")
  residualsTable$position <- position

  residualsTable$addColumnInfo(name = "type", title = "",                 type = "string")
  residualsTable$addColumnInfo(name = "min",  title = gettext("Minimum"), type = "number")
  residualsTable$addColumnInfo(name = "max",  title = gettext("Maximum"), type = "number")
  residualsTable$addColumnInfo(name = "mean", title = gettext("Mean"),    type = "number")
  residualsTable$addColumnInfo(name = "SD",   title = gettext("SD"),      type = "number")
  residualsTable$addColumnInfo(name = "N",    title = gettext("N"),       type = "integer")

  if (!is.null(finalModel))
    residualsTable$addRows(.linregGetResidualsStatistics(finalModel$fit, finalModel$predictors))

  modelContainer[["residualsTable"]] <- residualsTable
}

.linregCreateResidualsVsDependentPlot <- function(modelContainer, finalModel, options, position) {
  residVsDepPlot <- createJaspPlot(title = gettext("Residuals vs. Dependent"), width = 530, height = 400)
  residVsDepPlot$dependOn("plotResidualsDependent")
  residVsDepPlot$position <- position

  modelContainer[["residualsVsDepPlot"]] <- residVsDepPlot

  if (!is.null(finalModel) && !is.null(finalModel$fit)) {
    fit <- finalModel$fit
    .linregInsertPlot(residVsDepPlot, .linregPlotResiduals, xVar = fit$model[ , 1], xlab = options$dependent, res = residuals(fit), ylab = gettext("Residuals"))
  }
}

.linregCreateResidualsVsCovariatesPlots <- function(modelContainer, finalModel, dataset, options, position) {
  residualsVsCovContainer <- createJaspContainer(gettext("Residuals vs. Covariates"))
  residualsVsCovContainer$dependOn("plotResidualsCovariates")
  residualsVsCovContainer$position <- position
  modelContainer[["residualsVsCovContainer"]] <- residualsVsCovContainer

  if (!is.null(finalModel)) {
    predictors <- finalModel$predictors

    for (predictor in predictors)
      .linregCreatePlotPlaceholder(residualsVsCovContainer, index = .unvf(predictor), title = gettextf("Residuals vs. %s", .unvf(predictor)))

    for (predictor in predictors) {
      if (.linregIsInteraction(predictor) && .linregContainsFactor(finalModel$fit, predictor)) {
        # TODO: this is maybe possible when an interaction consists of only factors, but the plot won't be very pretty
        residualsVsCovContainer[[.unvf(predictor)]]$setError(gettext("Cannot plot residuals versus an interaction with a factor."))
      } else {
        .linregFillResidualsVsCovariatesPlot(residualsVsCovContainer[[.unvf(predictor)]], predictor, finalModel$fit, dataset)
      }
    }
  }
}

.linregFillResidualsVsCovariatesPlot <- function(residVsCovPlot, predictor, fit, dataset) {
  if (.linregIsInteraction(predictor))
    xVar <- .linregMakeCombinedVariableFromInteraction(predictor, dataset)
  else
    xVar <- dataset[[predictor]]

  .linregInsertPlot(residVsCovPlot, .linregPlotResiduals, xVar = xVar, xlab = .unvf(predictor), res = residuals(fit), ylab = gettext("Residuals"))
}

.linregCreateResidualsVsPredictedPlot <- function(modelContainer, finalModel, options, position) {
  residVsPredPlot <- createJaspPlot(title = gettext("Residuals vs. Predicted"), width = 530, height = 400)
  residVsPredPlot$dependOn("plotResidualsPredicted")
  residVsPredPlot$position <- position

  modelContainer[["residualsVsPredPlot"]] <- residVsPredPlot

  if (!is.null(finalModel) && !is.null(finalModel$fit)) {
    fit <- finalModel$fit
    .linregInsertPlot(residVsPredPlot, .linregPlotResiduals, xVar = predict(fit), xlab = gettext("Predicted Values"), res = residuals(fit), ylab = gettext("Residuals"))
  }
}

.linregCreateResidualsVsHistogramPlot <- function(modelContainer, finalModel, options, position) {
  title <- gettext("Residuals Histogram")
  if (options$plotResidualsHistogramStandardized)
    title <- gettextf("Standardized %s", title)

  residVsHistPlot <- createJaspPlot(title = title, width = 530, height = 400)
  residVsHistPlot$dependOn(c("plotResidualsHistogram", "plotResidualsHistogramStandardized"))
  residVsHistPlot$position <- position

  modelContainer[["residualsVsHistPlot"]] <- residVsHistPlot

  if (!is.null(finalModel))
    .linregFillResidualsVsHistogramPlot(residVsHistPlot, finalModel$fit, options)
}

.linregFillResidualsVsHistogramPlot <- function(residVsHistPlot, fit, options) {
  if (!is.null(fit)) {

    residName <- gettext("Residuals")
    resid     <- residuals(fit)
    if (options$plotResidualsHistogramStandardized) {
      residName <- gettextf("Standardized %s", residName)
      resid     <- resid / sd(resid)
    }

    .linregInsertPlot(residVsHistPlot, .linregPlotResidualsHistogram, res = resid, resName = residName)
  }
}

.linregCreateResidualsQQPlot <- function(modelContainer, finalModel, options, position) {
  residQQPlot <- createJaspPlot(title = gettext("Q-Q Plot Standardized Residuals"), width = 400, height = 400)
  residQQPlot$dependOn("plotResidualsQQ")
  residQQPlot$position <- position

  modelContainer[["residualsQQPlot"]] <- residQQPlot

  if (!is.null(finalModel) && !is.null(finalModel$fit)) {
    fit <- finalModel$fit
    .linregInsertPlot(residQQPlot, .linregPlotQQresiduals, res = residuals(fit) / sd(residuals(fit)))
  }
}

.linregCreatePartialPlots <- function(modelContainer, dataset, options, position) {
  predictors <- .linregGetPredictors(options$modelTerms, encoded = TRUE)

  title <- ngettext(length(predictors), "Partial Regression Plot", "Partial Regression Plots")

  partialPlotContainer <- createJaspContainer(title)
  partialPlotContainer$dependOn(c("plotsPartialRegression", "plotsPartialConfidenceIntervals", "plotsPartialConfidenceIntervalsInterval",
                                  "plotsPartialPredictionIntervals", "plotsPartialPredictionIntervalsInterval"))
  partialPlotContainer$position <- position
  modelContainer[["partialPlotContainer"]] <- partialPlotContainer

  predictors <- .linregGetPredictors(options$modelTerms, encoded = TRUE)
  if (any(.linregIsInteraction(predictors))) {
    .linregCreatePlotPlaceholder(partialPlotContainer, index = "placeholder", title = "")
    partialPlotContainer$setError(gettext("Partial plots are not supported for models containing interaction terms"))
    return()
  }

  if (options$dependent != "" && length(predictors) > 0) {
    for (predictor in predictors)
      .linregCreatePlotPlaceholder(partialPlotContainer, index = .unvf(predictor), title = gettextf("%1$s vs. %2$s", options$dependent, .unvf(predictor)))

    for (predictor in predictors) {
      if (.linregContainsFactor(dataset, predictor)) {
        partialPlotContainer[[.unvf(predictor)]]$setError(gettext("Partial plots are not supported for factors"))
      } else {
        .linregFillPartialPlot(partialPlotContainer[[.unvf(predictor)]], predictor, predictors, dataset, options)
      }
    }
  }
}

.linregFillPartialPlot <- function(partialPlot, predictor, predictors, dataset, options) {
  plotData  <- .linregGetPartialPlotData(predictor, predictors, dataset, options)
  xVar      <- plotData[["residualsPred"]]
  resid     <- plotData[["residualsDep"]]
  dfResid   <- length(resid) - length(predictors) - 1

  xlab      <- gettextf("Residuals %s", .unvf(predictor))
  ylab      <- gettextf("Residuals %s", options$dependent)

  # Compute regresion lines
  weights <- dataset[[.v(options$wlsWeights)]]
  line <- as.list(setNames(lm(residualsDep~residualsPred, data = plotData, weights = weights)$coeff,
                           c("intercept", "slope"))
                  )

  .linregInsertPlot(partialPlot, .linregPlotResiduals, xVar = xVar, res = resid, dfRes = dfResid, xlab = xlab, ylab = ylab,
                    regressionLine = TRUE, confidenceIntervals = options$plotsPartialConfidenceIntervals,
                    confidenceIntervalsInterval = options$plotsPartialConfidenceIntervalsInterval,
                    predictionIntervals = options$plotsPartialPredictionIntervals,
                    predictionIntervalsInterval = options$plotsPartialPredictionIntervalsInterval,
                    standardizedResiduals = FALSE, intercept = line[['intercept']], slope = line[['slope']])
}

.linregCreateDescriptivesTable <- function(modelContainer, dataset, options, position) {
  descriptivesTable <- createJaspTable(gettext("Descriptives"))
  descriptivesTable$dependOn("descriptives")
  descriptivesTable$position <- position

  descriptivesTable$addColumnInfo(name = "var",  title = "",              type = "string")
  descriptivesTable$addColumnInfo(name = "N",    title = gettext("N"),    type = "integer")
  descriptivesTable$addColumnInfo(name = "mean", title = gettext("Mean"), type = "number")
  descriptivesTable$addColumnInfo(name = "SD",   title = gettext("SD"),   type = "number")
  descriptivesTable$addColumnInfo(name = "SE",   title = gettext("SE"),   type = "number")

  variables <- c(options$dependent, unlist(options$covariates))
  variables <- variables[variables != ""]
  if (length(variables) > 0)
    descriptivesTable$addRows(.linregGetDescriptives(variables, dataset))

  modelContainer[["descriptivesTable"]] <- descriptivesTable
}

.linregCalcModel <- function(modelContainer, dataset, options, ready) {
  if (!ready)
    return()

  if (!is.null(modelContainer[["model"]]))
    return(modelContainer[["model"]]$object)

  dependent         <- .v(options$dependent)
  predictorsInNull  <- .linregGetPredictors(options$modelTerms, modelType = "null")
  predictors        <- .linregGetPredictors(options$modelTerms, modelType = "alternative") # these include the null terms

  if (options$wlsWeights != "")
    weights <- dataset[[.v(options$wlsWeights)]]
  else
    weights <- rep(1, length(dataset[[dependent]]))

  if (options$method %in% c("backward", "forward", "stepwise") && length(predictors) > 0)
    model <- .linregGetModelSteppingMethod(dependent, predictors, predictorsInNull, dataset, options, weights)
  else
    model <- .linregGetModelEnterMethod(dependent, predictors, predictorsInNull, dataset, options, weights)

  for (i in seq_along(model)) {
    singleModel <- model[[i]]
    model[[i]][["title"]]         <- .linregGetModelTitle(singleModel$predictors, predictorsInNull, options$method, i)
    model[[i]][["summary"]]       <- .linregGetSummary(singleModel$fit)
    model[[i]][["durbinWatson"]]  <- .linregGetDurBinWatsonTestResults(singleModel$fit, options$wlsWeights)
    model[[i]][["rSquareChange"]] <- .linregGetrSquaredChange(singleModel$fit, i, model[1:i], options)
  }

  modelContainer[["model"]] <- createJaspState(model)

  return(model)
}

.linregGetModelSteppingMethod <- function(dependent, predictors, predictorsInNull, dataset, options, weights) {
  if (options$method == "backward")
    model <- .linregBackwardRegression(dependent, predictors, predictorsInNull, dataset, options, weights)
  else if (options$method == "forward")
    model <- .linregForwardRegression(dependent, predictors, predictorsInNull, dataset, options, weights)
  else # stepwise
    model <- .linregStepwiseRegression(dependent, predictors, predictorsInNull, dataset, options, weights)

  return(model)
}

.linregGetModelEnterMethod <- function(dependent, predictors, predictorsInNull, dataset, options, weights) {
  model <- list()

  formulaNull <- NULL
  if (length(predictorsInNull) > 0)
    formulaNull <- .linregGetFormula(dependent, predictorsInNull, options$includeConstant)
  else if (options$includeConstant)
    formulaNull <- .linregGetFormula(dependent, NULL, TRUE)

  formula <- NULL
  if (length(predictors) > 0 && !all(predictors %in% predictorsInNull))
    formula <- .linregGetFormula(dependent, predictors, options$includeConstant)

  if (!is.null(formulaNull)) {
    fitNull <- stats::lm(formulaNull, data = dataset, weights = weights, x = TRUE)
    model[[1]] <- list(fit = fitNull, predictors = predictorsInNull, title = gettextf("H%s", "\u2080"))
  }

  if (!is.null(formula)) {
    fit <- stats::lm(formula, data = dataset, weights = weights, x = TRUE)
    model[[length(model) + 1]] <- list(fit = fit, predictors = predictors, title = gettextf("H%s", "\u2081"))
  }

  return(model)
}

.linregBackwardRegression <- function(dependent, predictors, predictorsInNull, data, options, weights) {
  formula <- .linregGetFormula(dependent, predictors, options$includeConstant)
  fit     <- stats::lm(formula, data = data, weights = weights, x = TRUE)
  model   <- list(list(fit = fit, predictors = predictors))

  candidatePredictors <- setdiff(predictors, predictorsInNull)
  while (length(candidatePredictors) > 0) {
    prevModel <- model[[length(model)]]
    nextModel <- .linregTryToRemoveOnePredictor(prevModel, dependent, predictorsInNull, data, options, weights)

    if (is.null(nextModel))
      break

    model[[length(model) + 1]]  <- nextModel
    candidatePredictors         <- setdiff(nextModel$predictors, predictorsInNull)
  }

  return(model)
}

.linregForwardRegression <- function(dependent, predictors, predictorsInNull, data, options, weights) {
  model <- list()
  if (options$includeConstant || length(predictorsInNull) > 0) {
    formula <- .linregGetFormula(dependent, predictorsInNull, options$includeConstant)
    fit     <- stats::lm(formula, data = data, weights = weights, x = TRUE)
    model   <- list(list(fit = fit, predictors = predictorsInNull))
  }

  candidatePredictors <- setdiff(predictors, predictorsInNull)
  while (length(candidatePredictors) > 0) {
    if (length(model) == 0)
      prevModel <- NULL
    else
      prevModel <- model[[length(model)]]

    nextModel <- .linregTryToAddOnePredictor(prevModel, dependent, candidatePredictors, predictorsInNull, data, options, weights)

    if (is.null(nextModel))
      break

    model[[length(model) + 1]]  <- nextModel
    candidatePredictors         <- setdiff(candidatePredictors, nextModel$predictors)
  }

  # we don't have an intercept or nuisance variables and no predictor could be added
  if (length(model) == 0)
    model <- list(list(fit = NULL, predictors = NULL))

  return(model)
}

.linregStepwiseRegression <- function(dependent, predictors, predictorsInNull, data, options, weights) {
  model <- list()
  if (options$includeConstant || length(predictorsInNull) > 0) {
    formula <- .linregGetFormula(dependent, predictorsInNull, options$includeConstant)
    fit     <- stats::lm(formula, data = data, weights = weights, x = TRUE)
    model   <- list(list(fit = fit, predictors = predictorsInNull))
  }

  predictorsNotInNull <- setdiff(predictors, predictorsInNull)
  candidatePredictors <- predictorsNotInNull
  while (length(candidatePredictors) > 0) {
    if (length(model) == 0)
      prevToAddModel <- NULL
    else
      prevToAddModel <- model[[length(model)]]

    addStepModel <- .linregTryToAddOnePredictor(prevToAddModel, dependent, candidatePredictors, predictorsInNull, data, options, weights)

    # stop if we cant even perform a single add step
    if (is.null(addStepModel) && all(prevToAddModel[["predictors"]] %in% predictorsInNull))
      break

    if (!is.null(addStepModel))
      model[[length(model) + 1]] <- addStepModel

    prevToRemoveModel <- model[[length(model)]]
    removeStepModel   <- .linregTryToRemoveOnePredictor(prevToRemoveModel, dependent, predictorsInNull, data, options, weights)

    # stop if no predictor could be added or removed
    if (is.null(addStepModel) && is.null(removeStepModel))
      break

    if (!is.null(removeStepModel))
      model[[length(model) + 1]] <- removeStepModel

    if (identical(prevToAddModel, removeStepModel))
      break

    candidatePredictors <- setdiff(predictorsNotInNull, model[[length(model)]]$predictors)
  }

  # we don't have an intercept or nuisance variables and no predictor could be added
  if (length(model) == 0)
    model <- list(list(fit = NULL, predictors = NULL))

  return(model)
}

.linregTryToAddOnePredictor <- function(prevModel = NULL, dependent, candidatePredictors, predictorsInNull, data, options, weights) {
  fValues <- numeric(length(candidatePredictors))
  pValues <- numeric(length(candidatePredictors))

  for (i in seq_along(candidatePredictors)) {
    formula <- .linregGetFormula(dependent, c(prevModel$predictors, candidatePredictors[i]), options$includeConstant)
    fit     <- lm(formula, data = data, weights = weights, x = TRUE)
    fValue  <- summary(fit)$coefficients[, "t value"]
    pValue  <- summary(fit)$coefficients[, "Pr(>|t|)"]

    if (length(fValue) > 1)
      fValue <- fValue[names(fValue) == candidatePredictors[i]]

    if (length(pValue) > 1)
      pValue <- pValue[names(pValue) == candidatePredictors[i]]

    fValues[i] <- fValue^2
    pValues[i] <- pValue
  }

  nextPredictors <- NULL
  if (options$steppingMethodCriteriaType == "useFValue" && max(fValues) > options$steppingMethodCriteriaFEntry) {
      highestFvaluePredictor <- candidatePredictors[which.max(fValues)]
      nextPredictors <- c(prevModel$predictors, highestFvaluePredictor)
  } else if (options$steppingMethodCriteriaType == "usePValue" && min(pValues) < options$steppingMethodCriteriaPEntry) {
      minimumPvalueVariable <- candidatePredictors[which.min(pValues)]
      nextPredictors <- c(prevModel$predictors, minimumPvalueVariable)
  }

  # no predictors could be added; the algorithm is done
  if (is.null(nextPredictors))
    return()

  formula <- .linregGetFormula(dependent, nextPredictors, options$includeConstant)
  fit     <- stats::lm(formula, data = data, weights = weights, x = TRUE)

  return(list(fit = fit, predictors = nextPredictors))
}

.linregTryToRemoveOnePredictor <- function(prevModel, dependent, predictorsInNull, data, options, weights) {
  tValues <- summary(prevModel[["fit"]])$coefficients[ , "t value"]
  pValues <- summary(prevModel[["fit"]])$coefficients[ , "Pr(>|t|)"]

  if (options$includeConstant) {
    tValues <- tValues[-1]
    pValues <- pValues[-1]
  }

  tValues <- tValues[!(names(tValues) %in% predictorsInNull)]
  pValues <- pValues[!(names(pValues) %in% predictorsInNull)]
  fValues <- tValues^2

  nextPredictors <- prevModel[["predictors"]]
  if (options$steppingMethodCriteriaType == "useFValue" && min(fValues) < options$steppingMethodCriteriaFRemoval) {
      lowestFvaluePredictor <- names(which.min(fValues))
      nextPredictors        <- prevModel$predictors[prevModel$predictors != lowestFvaluePredictor]
  } else if (options$steppingMethodCriteriaType == "usePValue" && max(pValues) > options$steppingMethodCriteriaPRemoval) {
      highestPvaluePredictor  <- names(which.max(pValues))
      nextPredictors          <- prevModel$predictors[prevModel$predictors != highestPvaluePredictor]
  }

  # no more predictors could be removed, the algorithm is done
  if (identical(nextPredictors, prevModel[["predictors"]]))
    return()

  if (length(nextPredictors) > 0)
    formula <- .linregGetFormula(dependent, nextPredictors, options$includeConstant)
  else if (length(predictorsInNull) > 0)
    formula <- .linregGetFormula(dependent, predictorsInNull, options$includeConstant)
  else if (options$includeConstant)
    formula <- .linregGetFormula(dependent, NULL, TRUE)
  else
    return() # we can't compute a null model because it has no intercept/nuisance variables, the algorithm is done

  fit <- stats::lm(formula, data = data, weights = weights, x = TRUE)

  return(list(fit = fit, predictors = nextPredictors))
}

.linregGetSummary <- function(fit) {
  summary <- list(r.squared = NaN, r.squared = NaN, adj.r.squared = NaN, sigma = NaN)

  if (!is.null(fit))
    summary <- summary(fit)

  return(summary)
}

.linregGetDurBinWatsonTestResults <- function(fit, weights) {
  durbinWatson <- list(r = NaN, dw = NaN, p = NaN)

  if (!is.null(fit)) {
    durbinWatson <- .durbinWatsonTest.lm(fit, alternative = c("two.sided"))

    if (weights == "") # if regression is not weighted, calculate p-value with lmtest (car method is unstable)
      # TODO: this can fail when there are many interactions between factors. Do we want to show a footnote about that?
      durbinWatson[["p"]] <- tryCatch(
        lmtest::dwtest(fit, alternative = c("two.sided"))$p.value,
        error = function(e) NaN
      )
  }

  return(durbinWatson)
}

.linregGetrSquaredChange <- function(fit, currentIndex, processedModels, options) {
  #R^2_change in Field (2013), Eqn. 8.15:
  #F.change = (n-p_new - 1)R^2_change / p_change ( 1- R^2_new)
  #df1 = p_change = abs( p_new - p_old )
  #df2 = n-p_new
  # the above works only for continuous predictors, for categorical, we have (k-1) coefficients
  # where k=number of the levels in the categorical variable

  if (currentIndex == 1) {
    # if we include the intercept, the number of coefficients to compare the null model to is 1
    # otherwise there are no coefficients
    prevCoefs    <- if(options[["includeConstant"]]) numeric(1) else numeric(0)
    prevRSquared <- 0
  } else {
    prevCoefs    <- stats::coefficients(processedModels[[currentIndex - 1]]$fit)
    prevRSquared <- summary(processedModels[[currentIndex - 1]]$fit)$r.squared
  }

  rSquaredChange <- fChange <- df1 <- df2 <- p <- NaN
  if (!is.null(fit)) {
    rSquared        <- summary(fit)$r.squared
    rSquaredChange  <- rSquared - prevRSquared

    coefs <- stats::coefficients(fit)

    df1 <- abs(length(coefs) - length(prevCoefs)) # df1 = p_change = abs( p_new - p_old )
    df2 <- stats::df.residual(fit) # df2 = n-p_new but should take factors into account

    if (df1 > 0L) {
      fChange <- (df2 * rSquaredChange) / (df1 * (1 - rSquared))
      p       <- pf(q = fChange, df1 = df1, df2 = df2, lower.tail = FALSE)
    } else {
      fChange <- p <- NA
    }
  }

  rSquareChange <- list(
    R2c = rSquaredChange,
    Fc  = fChange,
    df1 = df1,
    df2 = df2,
    p   = p
  )

  return(rSquareChange)
}

.linregGetAnova <- function(fit, predictors) {
  Fvalue <- mssResidual <- mssModel <- dfResidual <- dfModel <- dfTotal <- ssResidual <- ssModel <- ssTotal <- p <- vovksellke <- NaN

  if (!is.null(fit)) {
    if (length(predictors) > 0) {
      summary     <- summary(fit)

      Fvalue			<- summary$fstatistic[1]
      mssResidual	<- summary$sigma^2
      mssModel	  <- Fvalue * mssResidual
      dfResidual	<- summary$fstatistic[3]
      dfModel		  <- summary$fstatistic[2]
      dfTotal		  <- dfResidual + dfModel
      ssResidual  <- mssResidual * dfResidual
      ssModel		  <- mssModel * dfModel
      ssTotal		  <- ssResidual + ssModel

      p           <- pf(q = Fvalue, df1 = dfModel, df2 = dfResidual, lower.tail = FALSE)
      vovksellke  <- VovkSellkeMPR(p)
    } else {
      Fvalue <- mssResidual <- mssModel <- dfResidual <- dfModel <- dfTotal <- ssResidual <- ssModel <- ssTotal <- p <- vovksellke <- "."
    }
  }

  anova <- list(
    Regression  = list(F = Fvalue,  SS = ssModel,     df = dfModel,     MS = mssModel,  p = p, vovksellke = vovksellke),
    Residual    = list(             SS = ssResidual,  df = dfResidual,  MS = mssResidual),
    Total       = list(             SS = ssTotal,     df = dfTotal)
  )

  return(anova)
}

.linregGetCoefficients <- function(fit, predictors, dataset, options) {
  rows <- list()

  if (!is.null(fit)) {
    if (options$includeConstant)
      predictors <- c("(Intercept)", predictors)

    factors <- options[["factors"]]
    hasFactors <- length(factors) > 0L

    summ          <- summary(fit)
    missingCoeffs <- summ[["aliased"]]

    # adapted from stats:::print.summary.lm -- automatically handles missing values
    estimates <- matrix(NaN, length(missingCoeffs), 4, dimnames = list(names(missingCoeffs), colnames(coef(summ))))
    estimates[!missingCoeffs, ] <- coef(summ)
    confInterval  <- confint(fit, level = options$regressionCoefficientsConfidenceIntervalsInterval)
    confInterval[is.na(confInterval)] <- NaN

    info <- .linregGetParametersAndLevels(fit)
    names <- .linregMakePrettyNames(info)
    rawNames <- info[["paramsRaw"]]
    names[c(FALSE, names[-1L] == names[-length(names)])] <- ""

    # show footnote for missing coefficients
    footnote <- NULL
    if (any(missingCoeffs))
      footnote <- list(rows = names[missingCoeffs])


    rows <- vector("list", nrow(estimates))

    if (length(predictors) > 1 || predictors != "(Intercept)") {
      appropriatePredictors <- predictors[predictors != "(Intercept)"]
      appropriatePredictors <- .linregRemoveFactors(fit, appropriatePredictors)
      if (length(appropriatePredictors) > 0L)
        collinearityDiagnostics <- .linregGetVIFAndTolerance(appropriatePredictors, dataset, includeConstant = TRUE)
    }

    dataClasses <- attr(terms(fit), "dataClasses")
    rowIndex <- 1L

    for (i in seq_along(names)) {

      unstandCoeff <- estimates[i, "Estimate"]
      if (!identical(rawNames[[i]], "(Intercept)") && !any(dataClasses[rawNames[[i]]] == "factor")) {
        # these don't exist for the intercept or categorical predictors

        predictor <- paste(rawNames[[i]], collapse = ":")

        standCoeff <- .linregGetStandardizedCoefficient(dataset, options[["dependent"]], predictor, unstandCoeff)
        tolerance  <- collinearityDiagnostics[["tolerance"]][[predictor]]
        VIF        <- collinearityDiagnostics[["VIF"]][[predictor]]

      } else {

        standCoeff <- tolerance  <- VIF <- NULL

      }

      row <- list(
        name         = names[i],
        unstandCoeff = unstandCoeff,
        SE           = estimates[i, "Std. Error"],
        t            = estimates[i, "t value"],
        p            = estimates[i, "Pr(>|t|)"],
        lower        = confInterval[i, 1],
        upper        = confInterval[i, 2],
        standCoeff   = standCoeff,
        tolerance    = tolerance,
        VIF          = VIF,
        vovksellke   = VovkSellkeMPR(estimates[i, "Pr(>|t|)"])
      )

      rows[[i]] <- row

    }

  }

  return(list(coefficients = rows, footnote = footnote))
}

.linregGetBootstrapCoefficients <- function(fit, dataset, options) {
  .bootstrapping <- function(data, indices, formula, wlsWeights) {
    progressbarTick()

    d <- data[indices, , drop = FALSE] # allows boot to select sample
    if (wlsWeights == "") {
      fit <- lm(formula = formula, data = d)
    } else {
      weights <- d[[wlsWeights]]
      fit <- lm(formula = formula, data = d, weights = weights)
    }

    return(coef(fit))
  }

  data <- data.frame(unstandCoeff = numeric(0), bias = numeric(0), SE = numeric(0), lower = numeric(0), upper = numeric(0))

  if (!is.null(fit)) {

    missingCoeffs <- NULL
    coefNames <- names(coef(fit))
    if (anyNA(fit$coefficients))
      missingCoeffs <- coefNames[which(is.na(coef(fit)))]

    summary <- boot::boot(data = dataset, statistic = .bootstrapping,
                          R = options$regressionCoefficientsBootstrappingReplicates,
                          formula = formula(fit),
                          wlsWeights = options$wlsWeights)

    coefficients  <- matrixStats::colMedians(summary$t, na.rm = TRUE)
    bias          <- colMeans(summary$t, na.rm = TRUE) - summary$t0
    stdErrors     <- matrixStats::colSds(summary$t, na.rm = TRUE)

    for (i in seq_along(coefNames)) {
      coefName <- coefNames[[i]]

      if (coefName %in% missingCoeffs) {
        data[i, ] <- rep(NaN, ncol(data))
        next
      }

      ci <- try(boot::boot.ci(summary, type = "bca", conf = options$regressionCoefficientsConfidenceIntervalsInterval, index = i))
      if (jaspBase::isTryError(ci))
        ci <- list(bca = rep(NaN, 5L))

      data[i, ] <- c(coefficients[i], bias[i], stdErrors[i], ci$bca[4], ci$bca[5])
    }

    data[["name"]] <- .linregMakePrettyNames(fit)
  }

  return(data)
}

.linregGetVIFAndTolerance <- function(predictors, dataset, includeConstant) {
  VIF       <- list()
  tolerance <- list()
  if (length(predictors) > 1) {
    for (predictor in predictors) {

      if (.linregIsInteraction(predictor)) {
        newVar                <- .linregMakeCombinedVariableFromInteraction(predictor, dataset)
        newVarName            <- gsub(":", ".", predictor, fixed = TRUE)
        dataset[[newVarName]] <- newVar
      } else {
        newVarName            <- predictor
      }

      cleanedPredictors <- predictors[-which(predictors == predictor)]
      formula           <- .linregGetFormula(newVarName, cleanedPredictors, includeConstant = TRUE)
      fitVIF            <- stats::lm(formula, data = dataset)

      VIF[[predictor]]        <- 1 / (1 - summary(fitVIF)$"r.squared")
      tolerance[[predictor]]  <- 1 / VIF[[predictor]]
    }
  } else {
    VIF[[predictors]]       <- 1
    tolerance[[predictors]] <- 1
  }

  result <- list(VIF       = VIF,
                 tolerance = tolerance)

  return(result)
}

.linregGetStandardizedCoefficient <- function(dataset, dependent, predictor, unstandCoeff) {
  sdDependent <- sd(dataset[[dependent]])
  if (.linregIsInteraction(predictor))
    sdIndependent <- sd(.linregMakeCombinedVariableFromInteraction(predictor, dataset))
  else
    sdIndependent <- sd(dataset[[predictor]])

  return(unstandCoeff * sdIndependent / sdDependent)
}

.linregGetPartAndPartialCorrelation <- function(fit, predictors, dataset, options) {
  formula <- formula(fit)
  R2      <- summary(fit)[["r.squared"]]

  cors <- vector("list", length(predictors))
  names(cors) <- predictors

  for(predictor in predictors) {

    # drop the term from the formula and refit the model
    newFormula <- update(formula, as.formula(sprintf(". ~ . - %s", predictor)))
    newFit     <- update(fit, formula = newFormula)
    newR2      <- summary(newFit)[["r.squared"]]

    sr2 <- R2 - newR2      # squared semi-partial (part) correlation
    pr2 <- sr2 / (1-newR2) # squared partial correlation

    # determine the sign of the coefficient
    sign <- sign(coefficients(fit)[predictor])
    if(is.na(sign)) sign <- 1 # for categorical predictors

    cors[[predictor]] <- list(
      name    = predictor,
      part    = sign * sqrt(sr2),
      partial = sign * sqrt(pr2)
    )
  }

  return(cors)
}

.linregGetCovarianceMatrix <- function(fit, rawNames, prettyNames) {
  data <- data.frame()

  if (!is.null(fit)) {

    namesInModel <- setdiff(names(coef(fit)), "(Intercept)")
    covmatrix <- matrix(NA_real_, length(namesInModel), length(rawNames), dimnames = list(namesInModel, rawNames))
    covmatrix[namesInModel, namesInModel] <- vcov(fit)[namesInModel, namesInModel]
    covmatrix[lower.tri(covmatrix)] <- NA

    if (nrow(covmatrix) > 0L) {
      colnames(covmatrix) <- prettyNames
      names <- prettyNames[rawNames %in% namesInModel]
      data <- cbind(data.frame(name = names), covmatrix)
    }
  }

  return(data)
}

.linregGetCollinearityDiagnostics <- function(fit, columns, includeConstant) {
  data <- data.frame()

  if (!is.null(fit)) {
    eigenvalues         <- .linregGetEigenValues(fit)
    conditionIndices    <- .linregGetConditionIndices(fit)
    varianceProportions <- .linregGetVarianceProportions(fit)

    data <- data.frame(dimension = seq_along(names(fit$coefficients)), eigenvalue = eigenvalues, condIndex = conditionIndices)
    colnames(varianceProportions) <- .linregMakePrettyNames(fit)
    data <- cbind(data, varianceProportions)
  }

  return(data)
}

.linregGetEigenValues <- function(fit) {
  X           <- .linregGetScaledPredictorMatrix(fit)
  eigenvalues <- svd(X)$d^2 # see Liao & Valliant (2012)

  return(eigenvalues)
}

.linregGetConditionIndices <- function(fit) {
  eigenvalues       <- .linregGetEigenValues(fit)
  conditionIndices  <- sqrt(max(eigenvalues) / eigenvalues)

  return(conditionIndices)
}

.linregGetVarianceProportions <- function(fit) {
  X <- .linregGetScaledPredictorMatrix(fit)

  ### ( see e.g., Liao & Valliant, 2012 )
  svdX  <- svd(X) # singular value decomposition
  M     <- svdX$v %*% solve(diag(svdX$d))
  Q     <- M*M # Hadamard (elementwise) product
  tQ    <- t(Q)

  for (i in seq_len(ncol(tQ)))
    tQ[ , i] <- tQ[ , i] / sum(tQ[ , i])

  colnames(tQ) <- names(fit$coefficients)

  return(tQ)
}

.linregGetScaledPredictorMatrix <- function(fit) {
  X <- fit$x

  for (i in seq_len(ncol(X)))
    X[ , i] <- X[ , i] / sqrt(sum(X[ , i]^2)) # scale each column using Euclidean norm

  return(X)
}

.linregGetCasewiseDiagnostics <- function(fit, options) {
  diagnostics <- list()

  if (!is.null(fit)) {
    predictedValuesAll    <- predict(fit)
    residualsAll          <- residuals(fit)
    stdPredictedValuesAll <- (predictedValuesAll - mean(predictedValuesAll)) / sd(predictedValuesAll)
    stdResidualsAll       <- rstandard(fit)
    cooksDAll             <- cooks.distance(fit)

    if (options$residualsCasewiseDiagnosticsType == "cooksDistance")
      index <- which(abs(cooksDAll) > options$residualsCasewiseDiagnosticsCooksDistance)
    else if (options$residualsCasewiseDiagnosticsType == "outliersOutside")
      index <- which(abs(stdResidualsAll) > options$residualsCasewiseDiagnosticsOutliersOutside)
    else # all
      index <- seq_along(predictedValuesAll)

    if (length(index) > 0) {
      diagnostics[["caseNumber"]]   <- index
      diagnostics[["stdResidual"]]  <- stdResidualsAll[index]
      diagnostics[["dependent"]]    <- fit$model[index, 1]
      diagnostics[["predicted"]]    <- predictedValuesAll[index]
      diagnostics[["residual"]]     <- residualsAll[index]
      diagnostics[["cooksD"]]       <- cooksDAll[index]
    }
  }

  return(diagnostics)
}

.linregGetResidualsStatistics <- function(fit, predictors) {
  residuals <- list()

  if (!is.null(fit)) {
    typesTranslated <- list("Predicted Value"=gettext("Predicted Value"), "Residual"=gettext("Residual"), "Std. Predicted Value"=gettext("Std. Predicted Value"), "Std. Residual"=gettext("Std. Residual"))
    types           <- names(typesTranslated)

    predicted     <- predict(fit)
    N             <- length(predicted)
    valuesPerType <- list("Predicted Value"       = predicted,
                          "Residual"              = residuals(fit),
                          "Std. Predicted Value"  = (predicted - mean(predicted)) / sd(predicted),
                          "Std. Residual"         = rstandard(fit))

    if (length(predictors) == 0)
      valuesPerType[["Std. Predicted Value"]] <- NA # cannot compute this for an intercept model

    residuals <- vector("list", length(types))
    for (i in seq_along(types)) {
      residuals[[i]]  <- list()
      type            <- types[i]

      residuals[[i]][["type"]] <- typesTranslated[[type]]
      residuals[[i]][["min"]]  <- min( valuesPerType[[type]], na.rm = TRUE)
      residuals[[i]][["max"]]  <- max( valuesPerType[[type]], na.rm = TRUE)
      residuals[[i]][["mean"]] <- mean(valuesPerType[[type]], na.rm = TRUE)
      residuals[[i]][["SD"]]   <- sd(  valuesPerType[[type]], na.rm = TRUE)
      residuals[[i]][["N"]]    <- N
    }
  }

  return(residuals)
}

.linregGetDescriptives <- function(variables, dataset) {
  descriptives <- vector("list", length(variables))

  for (i in seq_along(variables)) {
    descriptives[[i]] <- list()

    variable  <- variables[i]
    data      <- na.omit(dataset[[.v(variable)]])

    descriptives[[i]][["var"]]  <- variable
    descriptives[[i]][["N"]]    <- length(data)
    descriptives[[i]][["mean"]] <- mean(data)
    descriptives[[i]][["SD"]]   <- sd(data)
    descriptives[[i]][["SE"]]   <- sd(data) / sqrt(length(data))
  }

  return(descriptives)
}

.linregGetPartialPlotData = function(predictor, predictors, dataset, options) {
  predictors <- setdiff(predictors, predictor)
  if (length(predictors) == 0)
    predictors <- NULL

  weights <- dataset[[options$wlsWeights]]

  # Compute residuals dependent
  formulaDep    <- .linregGetFormula(options$dependent, predictors = predictors, includeConstant = TRUE)
  fitDep        <- stats::lm(formula = formulaDep, data = dataset, weights = weights)
  residualsDep  <- residuals(fitDep)

  # Compute residuals predictor as dependent
  formulaPred   <- .linregGetFormula(predictor, predictors = predictors, includeConstant = TRUE)
  fitPred       <- stats::lm(formula = formulaPred, data = dataset, weights = weights)
  residualsPred <- residuals(fitPred)

  return(data.frame(residualsPred = residualsPred, residualsDep = residualsDep))
}

.linregPlotResiduals <- function(xVar = NULL, res = NULL, dfRes = Inf, xlab, ylab = gettext("Residuals"), cexPoints= 1.3, cexXAxis= 1.3, cexYAxis= 1.3, lwd= 2, lwdAxis=1.2,
                                 regressionLine = TRUE, confidenceIntervals = FALSE, confidenceIntervalsInterval = 0.95, predictionIntervals = FALSE, predictionIntervalsInterval = 0.95,
                                 standardizedResiduals = TRUE, intercept = 0, slope = 0) {

  # TODO: slope should consist of multiple values for factors with more than 2 levels
  d     <- data.frame(xx= xVar, yy= res)
  d     <- na.omit(d)
  xVar  <- d$xx
  res   <- d$yy

  # construct here the x-axis scale which can be categorical or continuous
  if (is.factor(xVar)) {
    xScale <- ggplot2::scale_x_discrete(name = xlab)
  } else {
    xlow   <- min(pretty(xVar))
    xhigh  <- max(pretty(xVar))
    xticks <- pretty(c(xlow, xhigh))
    xlabs  <- jaspGraphs::axesLabeller(xticks, digits = 3)
    xScale <- ggplot2::scale_x_continuous(name = xlab, breaks = xticks, labels = xlabs)
  }

  # y-axis scale is always continuous (since the dependent variable in linear regression should be continous)
  ylow   <- min(pretty(res))
  yhigh  <- max(pretty(res))
  yticks <- pretty(c(ylow, yhigh, 0))
  ylabs  <- jaspGraphs::axesLabeller(yticks, digits = 3)
  ylim   <- range(yticks)

  if (standardizedResiduals) {

    stAxisTmp               <- pretty( yticks / sd(res) )
    stAxisOriginalScaleTmp  <- stAxisTmp * sd(res)
    stAxisOriginalScale     <- stAxisOriginalScaleTmp[stAxisOriginalScaleTmp < max(yticks) & stAxisOriginalScaleTmp > min(yticks)]
    stAxis                  <- stAxisOriginalScale / sd(res)

    yScaleSecAxis <- ggplot2::sec_axis(~.+0, breaks = stAxisOriginalScale, name = gettext("Standardized Residuals\n"),labels = stAxis)

  } else {
    yScaleSecAxis <- ggplot2::waiver()
  }

  yScale <- ggplot2::scale_y_continuous(name = ylab, breaks = yticks, labels = ylabs, limits = ylim, sec.axis = yScaleSecAxis)

  regLine <- confidenceIntervalLines <- predictionIntervalLines <- NULL
  if (regressionLine) {

    regLine <- if (is.factor(xVar)) {
      ggplot2::geom_line(
        data = data.frame(x = as.numeric(unique(xVar)), y = intercept + slope),
        mapping = ggplot2::aes(x = x, y = y),
        col = "darkred", size = .5
      )
    } else {
      ggplot2::geom_line(
        data = data.frame(x = c(min(xticks), max(xticks)), y = intercept + slope * c(min(xticks), max(xticks))),
        mapping = ggplot2::aes(x = x, y = y),
        col = "darkred", size = .5
      )
    }

    if (confidenceIntervals) {

      seConf <- sqrt(sum(res^2) / dfRes) *
        sqrt(1 / length(res) + (xVar - mean(xVar))^2 / sum((xVar - mean(xVar))^2))

      ciConf <- 1 - (1 - confidenceIntervalsInterval)/2

      upperConfInt <- (intercept + slope * xVar) + qt(ciConf, dfRes) * seConf
      lowerConfInt <- (intercept + slope * xVar) - qt(ciConf, dfRes) * seConf

      # ggplot2::geom_errorbar()

      confidenceIntervalLines <- if (is.factor(xVar)) {
        ggplot2::geom_errorbar(
          data = data.frame(x = levels(xVar), ymax = upperConfInt, ymin = lowerConfInt),
          mapping = ggplot2::aes(x = x, ymax = ymax, ymin = ymin),
          colour = "darkblue", linetype = "dashed"
        )
      } else {
        ggplot2::geom_line(
          data = data.frame(x = xVar, y = c(upperConfInt, lowerConfInt), g = rep(1:2, c(length(upperConfInt), length(lowerConfInt)))),
          mapping = ggplot2::aes(x = x, y = y, group = g),
          colour = "darkblue", linetype = "dashed"
        )
      }

    }

    if (predictionIntervals) {

      sePred <- sqrt(sum(res^2) / dfRes) *
        sqrt(1 + 1 / length(res) + (xVar - mean(xVar))^2 / sum((xVar - mean(xVar))^2))

      ciPred <- 1 - (1 - predictionIntervalsInterval)/2

      upperPredInt <- (intercept + slope * xVar) + qt(ciPred, dfRes) * sePred
      lowerPredInt <- (intercept + slope * xVar) - qt(ciPred, dfRes) * sePred

      predictionIntervalLines <- if (is.factor(xVar)) {
        ggplot2::geom_errorbar(
          data = data.frame(x = levels(xVar), ymax = upperPredInt, ymin = lowerPredInt),
          mapping = ggplot2::aes(x = x, ymax = ymax, ymin = ymin),
          colour = "darkgreen", linetype = "longdash"
        )
      } else {
        ggplot2::geom_line(data = data.frame(x = xVar, y = c(upperPredInt, lowerPredInt), g = rep(1:2, c(length(upperPredInt), length(lowerPredInt)))),
                           mapping = ggplot2::aes(x = x, y = y, group = g),
                           col = "darkblue", linetype = "dashed", size = 1)
      }
    }


  }

  residualPoints <- jaspGraphs::geom_point(data = data.frame(x = xVar, y = res), mapping = ggplot2::aes(x = x, y = y))

  p <- ggplot2::ggplot() +
    xScale + yScale +
    regLine +
    confidenceIntervalLines +
    predictionIntervalLines +
    residualPoints +
    jaspGraphs::geom_rangeframe(sides = if (standardizedResiduals) "blr" else "bl") +
    jaspGraphs::themeJaspRaw(axis.title.cex = 1.2)

  return(p)
}

.linregPlotResidualsHistogram <- function(res = NULL, resName = gettext("Residuals"), cexYlab= 1.3, lwd= 2, rugs= FALSE) {
  density <- density(res)

  h       <- hist(res, plot = FALSE)
  dens    <- density(res)
  yhigh   <- max(c(h$density, dens$y))
  ylow    <- 0
  xticks  <- base::pretty(c(res, h$breaks), min.n= 3)

  p <- ggplot2::ggplot() +
    ggplot2::scale_x_continuous(name = resName,            breaks = xticks,         labels = xticks, limits = range(xticks)) +
    ggplot2::scale_y_continuous(name = gettext("Density"), breaks = c(ylow, yhigh), labels = NULL) +
    ggplot2::geom_histogram(
      data = data.frame(res),
      mapping = ggplot2::aes(x = res, y = ..density..),
      binwidth = (h$breaks[2] - h$breaks[1]),
      fill = "grey", col = "black", size = .3,
      center = ((h$breaks[2] - h$breaks[1])/2)
    ) +
    ggplot2::geom_line(data = data.frame(x = density$x, y = density$y), mapping = ggplot2::aes(x = x, y = y), lwd = .7, col = "black") +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(axis.title.cex = 1.2) +
    ggplot2::theme(axis.ticks.y = ggplot2::element_blank())

  return(p)
}

.linregPlotQQresiduals <- function(res = NULL, xlab = gettext("Theoretical Quantiles"), ylab= gettext("Standardized Residuals"), cexPoints= 1.3, cexXAxis= 1.3, cexYAxis= 1.3, lwd= 2, lwdAxis=1.2) {
  d     <- data.frame(qqnorm(res, plot.it = FALSE))
  d     <- na.omit(d)
  xVar  <- d$x
  yVar  <- d$y

  xlow    <- min(pretty(xVar))
  xhigh   <- max(pretty(xVar))
  xticks  <- pretty(c(xlow, xhigh))

  ylow    <- min(pretty(yVar))
  yhigh   <- max(pretty(yVar))
  yticks  <- pretty(c(ylow, yhigh))

  yLabs <- vector("character", length(yticks))

  for (i in seq_along(yticks)) {
    if (yticks[i] < 10^6)
      yLabs[i] <- format(yticks[i], digits= 3, scientific = FALSE)
    else
      yLabs[i] <- format(yticks[i], digits= 3, scientific = TRUE)
  }

  p <- ggplot2::ggplot() +
    ggplot2::scale_x_continuous(name = gettext("Theoretical Quantiles"), breaks = xticks) +
    ggplot2::scale_y_continuous(name = gettext("Standardized Residuals"), breaks = xticks) +# TODO: why are xticks used here even though yticks exists?
    ggplot2::geom_line(
      data = data.frame(x = c(min(xticks), max(xticks)), y = c(min(xticks), max(xticks))),
      mapping = ggplot2::aes(x = x, y = y),
      col = "darkred", size = 1
    ) +
    jaspGraphs::geom_point(
      data = data.frame(x = xVar, y = yVar),
      mapping = ggplot2::aes(x = x, y = y)
    ) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(axis.title.cex = 1.2)

  return(p)
}

.linregGetPredictors <- function(modelTerms, modelType = "alternative", encoded = TRUE) {
  if (!is.character(modelType) || !modelType %in% c("alternative", "null"))
    stop(gettext("Unknown value provided for modelType, possible values: `alternative`, `null`"))

  predictors <- NULL
  for (i in seq_along(modelTerms)) {
    components <- unlist(modelTerms[[i]]$components)
    if (encoded)
      components <- .v(components)
    predictor <- paste0(components, collapse = ":")

    if (modelType == "alternative") {
      predictors <- c(predictors, predictor)
    } else if (modelType == "null") {
      isNuisance <- modelTerms[[i]]$isNuisance
      if (isNuisance)
        predictors <- c(predictors, predictor)
    }
  }

  return(predictors)
}

.reglinTermsContainNuisance <- function(modelTerms) {
  for (i in seq_along(modelTerms)) {
    isNuisance <- modelTerms[[i]]$isNuisance
    if (isNuisance)
      return(TRUE)
  }

  return(FALSE)
}

.linregGetFormula <- function(dependent, predictors = NULL, includeConstant) {
  if (is.null(predictors) && includeConstant == FALSE)
    stop(gettext("We need at least one predictor, or an intercept to make a formula"))

  if (is.null(predictors))
    formula <- paste(dependent, "~", "1")
  else if (includeConstant)
    formula <- paste(dependent, "~", paste(predictors, collapse = "+"))
  else
    formula <- paste(dependent, "~", paste(predictors, collapse = "+"), "-1")

  return(as.formula(formula, env = parent.frame(1)))
}

.linregGetModelTitle <- function(predictors, predictorsInNull, method, index) {
  modelTitle <- index
  if (method == "enter") {
    if (index == 1 && (length(predictors) == 0 || all(predictors %in% predictorsInNull)))
      modelTitle <- gettextf("H%s", "\u2080")
    else
      modelTitle <- gettextf("H%s", "\u2081")
  }

  return(modelTitle)
}

.linregIsInteraction <- function(predictor) {
  grepl(":", predictor)
}

.linregMakeCombinedVariableFromInteraction <- function(interaction, dataset) {
  terms   <- unlist(strsplit(interaction, split = ":"))
  newVar  <- rep(1, nrow(dataset))
  for (i in seq_along(terms))
    newVar <- newVar * dataset[[terms[i]]]

  return(newVar)
}

.linregGetPredictorColumnNames <- function(model, modelTerms) {
  usedPredictors  <- unique(unlist(lapply(model, function(x) x$predictors)))
  allpredictors   <- .linregGetPredictors(modelTerms)
  return(intersect(allpredictors, usedPredictors)) # ensures that the terms appear in the covariance matrix like they appear in the model terms box
}

.linregGetParameterNames <- function(model) {
  UseMethod(".linregGetParameterNames", model)
}

.linregGetParameterNames.lm <- function(model) {
  return(colnames(model.matrix(model)))
}

.linregGetParameterNames.list <- function(model) {
  usedParameterNames <- unique(unlist(lapply(model, function(x) .linregGetParameterNames(x[["fit"]])), use.names = FALSE))
  return(usedParameterNames)
}

.linregAddPredictorsAsColumns <- function(jaspTable, model, includeIntercept = TRUE, type = "number", format = NULL, overtitle = NULL) {

  titles <- .linregMakePrettyNames(model)
  if (!includeIntercept)
    titles <- titles[-1L]

  for (title in titles)
    jaspTable$addColumnInfo(name = title, title = title, type = type, format = format, overtitle = overtitle)

}

.linregAddPredictorsInNullFootnote <- function(jaspTable, modelTerms) {
  containsNuisance <- .reglinTermsContainNuisance(modelTerms)
  if (containsNuisance) {
    predictorsInNull <- .linregGetPredictors(modelTerms, modelType = "null", encoded = FALSE)
    jaspTable$addFootnote(message = gettextf("Null model includes %s", paste(predictorsInNull, collapse = ", "), sep = ""))
  }
}

.linregAddVovkSellke <- function(jaspTable, wantsVovkSellkeMPR) {
  if (wantsVovkSellkeMPR) {
    jaspTable$addColumnInfo(name = "vovksellke", title = gettext("VS-MPR"), type = "number")
    #Haven't I seen the following footnote before?
    jaspTable$addFootnote(symbol = "\u002A", colNames = "vovksellke", message = gettextf("Vovk-Sellke Maximum <em>p</em>-Ratio: Based on the <em>p</em>-value, the maximum
        possible odds in favor of H%1$s over H%2$s equals 1/(-e <em>p</em> log(<em>p</em>)) for <em>p</em> %3$s .37 (Sellke, Bayarri, & Berger, 2001).", "\u2081", "\u2080", "\u2264"))
  }
}

.linregAddInterceptNotShownFootnote <- function(jaspTable, model, options) {
  indicesOfModelsWithPredictors <- .linregGetIndicesOfModelsWithPredictors(model, options)
  if (options$includeConstant && length(indicesOfModelsWithPredictors) != length(model)) {
    if (length(indicesOfModelsWithPredictors) > 0)
      jaspTable$addFootnote(gettext("The intercept model is omitted, as no meaningful information can be shown."))
    else
      jaspTable$addFootnote(gettext("There is only an intercept model, no meaningful information can be shown."))
  }
}

.linregGetIndicesOfModelsWithPredictors <- function(model, options) {
  predictorsInNull  <- .linregGetPredictors(options$modelTerms, modelType = "null")
  indices           <- seq_along(model)
  if (options$method == "enter") {
    if (length(model) >= 1 && options$includeConstant && is.null(predictorsInNull))
      indices <- indices[-1]
  } else {
    for (i in seq_along(model))
      if (length(model[[i]]$predictors) == 0)
        indices <- indices[-i]
  }
  return(indices)
}

.linregInsertPlot <- function(jaspPlot, func, ...) {
  p <- try(func(...))

  if (inherits(p, "try-error")) {
   errorMessage <- .extractErrorMessage(p)
   jaspPlot$setError(gettextf("Plotting is not possible: %s", errorMessage))
  } else {
    jaspPlot$plotObject <- p
    jaspPlot$status     <- "complete"
  }
}

.linregCreatePlotPlaceholder <- function(container, index, title, width = 530, height = 400) {
  jaspPlot            <- createJaspPlot(title = title, width = width, height = height)
  jaspPlot$status     <- "running"
  container[[index]]  <- jaspPlot
}

.linregGetParametersAndLevels <- function(model, ...) {
  UseMethod(".linregGetParametersAndLevels", model)
}

.linregGetParametersAndLevels.lm <- function(model) {
  predictors <- all.vars(formula(model))[-1L]
  .linregGetParametersAndLevels(.linregGetParameterNames(model), predictors)
}

.linregGetParametersAndLevels.default <- function(model, predictors) {

  # exception for intercept only model
  if (length(predictors) == 0L && identical(model, "(Intercept)")) {
    return(list(
      paramsClean = model,
      levelsClean = "",
      paramsRaw   = model,
      levelsRaw   = ""
    ))
  }

  orderedPredictors <- predictors[order(nchar(predictors))]
  # escape the parentheses
  orderedPredictors[which(orderedPredictors == "(Intercept)")] <- "\\(Intercept\\)"
  regexAnyPredictor <- paste(orderedPredictors, collapse = "|")

  lvls <- gsub(regexAnyPredictor, "", model)

  levelsRaw   <- strsplit(lvls, ":")
  levelsRaw[lengths(levelsRaw) == 0] <- list("")
  # the above works except for n-way interactions between continuous predictors there needs to be one more empty level
  # so that the number of levels matches the number of predictors. The regex below finds these interactions and adds an
  # extra "" to the levels.
  for (i in grep("^(:+)$", lvls))
    levelsRaw[[i]] <- c(levelsRaw[[i]], "")

  if (length(levelsRaw[[1L]]) == 1L && levelsRaw[[1L]] == "(Intercept)")
    levelsRaw[[1L]] <- ""

  levelsClean <- jaspBase::gsubInteractionSymbol(lvls)

  splitRnms <- strsplit(model, ":")
  allPredsRegex <- paste0("^(", regexAnyPredictor, ").*")
  paramsRaw <- lapply(splitRnms, gsub, pattern = allPredsRegex, replacement = "\\1")
  paramsClean <- unlist(lapply(paramsRaw, paste, collapse = jaspBase::interactionSymbol), use.names = FALSE)
  return(list(
    paramsClean = paramsClean,
    levelsClean = levelsClean,
    paramsRaw   = paramsRaw,
    levelsRaw   = levelsRaw
  ))
}

#' Change R names into pretty names, e.g., PredictorLevel ->
#'
#' @param info either an lm object, a list where each sub element $fit contains an lm object, or the result of .linregGetParametersAndLevels.
#'
#' @details For example "PredictorLevel" becomes "Predictor" Level
.linregMakePrettyNames <- function(info) {
  UseMethod(".linregMakePrettyNames", info)
}

.linregMakePrettyNames.lm <- function(info) {
  .linregMakePrettyNames(.linregGetParametersAndLevels(info))
}

.linregMakePrettyNames.list <- function(info) {

  if (is.null(names(info)) && !is.null(info[[1L]][["fit"]])) {
    # we could also distinguish between these cases by giving the return value of .linregGetParametersAndLevels a class.
    return(unique(unlist(lapply(info, function(x) .linregMakePrettyNames.lm(x[["fit"]])), use.names = FALSE)))
  }

  title <- character(length(info[["paramsRaw"]]))
  for (j in seq_along(title)) {

    params <- info[["paramsRaw"]][[j]]
    levels <- info[["levelsRaw"]][[j]]

    ans <- character(length(params))
    for (i in seq_along(params)) {
      ans[i] <- if (levels[i] == "") params[i] else paste0(params[i], " (", levels[i], ")")
    }
    title[j] <- paste(ans, collapse = " \u2009\u273b\u2009 ")
  }
  return(title)
}

.linregRemoveFactors <- function(fit, predictors) {

  terms <- terms(fit)
  dataClasses <- attr(terms, "dataClasses")
  factors     <- attr(terms, "factors")
  result <- logical(length(predictors))
  for (i in seq_along(result)) {
    consistsOf <- names(which(factors[, predictors[i]] == 1))
    result[i] <- !any(dataClasses[consistsOf] == "factor")
  }
  return(predictors[result])
}

.linregContainsFactor <- function(x, predictors) {
  UseMethod(".linregContainsFactor", x)
}

.linregContainsFactor.data.frame <- function(x, predictors) {
  idx <- .linregIsInteraction(predictors)
  if (any(idx))
    predictors <- unique(unlist(strsplit(predictors, ":", fixed = TRUE), use.names = FALSE))

  return(any(vapply(x[predictors], is.factor, logical(1L))))

}

.linregContainsFactor.lm <- function(x, predictors) {
  # returns TRUE if the predictor is a factor or if it is an interaction that contains a factor
  terms <- terms(x)
  dataClasses <- attr(terms, "dataClasses")
  factors     <- attr(terms, "factors")
  consistsOf <- names(which(factors[, predictors] == 1))
  return(any(dataClasses[consistsOf] == "factor"))

}



.linregCreateMarginalPlots <- function(modelContainer, finalModel, dataset, options, position = 17) {
  marginalPlotsContainer <- createJaspContainer(gettext("Marginal Effects Plots"))
  marginalPlotsContainer$dependOn(c("plotsMarginal", "plotsMarginalConfidenceIntervals", "plotsMarginalConfidenceLevel",
                                    "plotsMarginalPredictionIntervals", "plotsMarginalPredictionLevel"))
  marginalPlotsContainer$position <- position
  modelContainer[["marginalPlotsContainer"]] <- marginalPlotsContainer

  if (!is.null(finalModel)) {
    predictors <- finalModel$predictors

    for (predictor in predictors)
      .linregCreatePlotPlaceholder(marginalPlotsContainer,
                                   index = predictor,
                                   title = gettextf("Marginal effect of %1s on %2s", predictor, options$dependent))

    for (predictor in predictors) {
      .linregFillMarginalPlots(marginalPlotsContainer[[predictor]], predictor, finalModel$fit, dataset, options)
    }
  }
}


.linregFillMarginalPlots <- function(marginalPlot, predictor, fit, dataset, options) {
  xVar <- dataset[[predictor]]
  xVar <- stats::na.omit(xVar)


  means_ls = list()
  if (length(options[["factors"]]) > 0) {
    for (var in options[["factors"]]) {
      column_value = dataset[[var]]
      column_levels = levels(column_value)
      means_ls[[var]] = column_levels[1]
    }
  }

  if (length(options[['covariates']]) > 0) {
    for (var in options[['covariates']]) {
      column_value = dataset[[var]]
      column_mean = mean(column_value, na.rm = TRUE)
      means_ls[[var]] = column_mean
    }
  }

  means_ls[[predictor]] = NULL

  dd_sim = data.frame(predictor = xVar)
  colnames(dd_sim) = predictor

  if (length(means_ls) > 0) {
    dd_sim = cbind(dd_sim, means_ls)
  }

  fitted = predict(fit, newdata = dd_sim, interval = "none")

  if (options$plotsMarginalConfidenceIntervals == TRUE) {
    matrix_conf = predict(fit,
                          newdata = dd_sim,
                          interval = "confidence",
                          level = options[["plotsMarginalConfidenceLevel"]])
    conf_min = matrix_conf[, 'lwr']
    conf_max = matrix_conf[, 'upr']
  }
  else {
    conf_min = NULL
    conf_max = NULL
  }

  if (options$plotsMarginalPredictionIntervals == TRUE) {
    matrix_pred = predict(fit,
                          newdata = dd_sim,
                          interval = "prediction",
                          level = options[["plotsMarginalPredictionLevel"]])
    pred_min = matrix_pred[, 'lwr']
    pred_max = matrix_pred[, 'upr']
  }
  else {
    pred_min = NULL
    pred_max = NULL
  }

  .linregInsertPlot(marginalPlot,
                    .linregMarginalPlot,
                    xVar = xVar,
                    xlab = predictor,
                    yVar = fitted,
                    ylab = options$dependent,
                    conf_min = conf_min,
                    conf_max = conf_max,
                    pred_min = pred_min,
                    pred_max = pred_max)
}


.linregMarginalPlot <- function(xVar, xlab, yVar, ylab,
                                conf_min, conf_max, pred_min, pred_max, options) {

  d <- data.frame(x = xVar,
                  y = yVar)

  if (is.factor(xVar)) {
    d_factor <- unique(d)
    d_factor <- cbind(d_factor, group = 1)

    basicMarginalPlot <- ggplot2::ggplot() +
      ggplot2::geom_line(data = d_factor,
                         mapping = ggplot2::aes(x = x, y = y, group = group),
                         size = 1) +
      ggplot2::xlab(xlab) +
      ggplot2::ylab(ylab)

    factorPoints <- jaspGraphs::geom_point(data = d_factor,
                                           mapping = ggplot2::aes(x = x, y = y))



  } else {
    basicMarginalPlot <- ggplot2::ggplot() +
      ggplot2::geom_line(data = d,
                         mapping = ggplot2::aes(x = x, y = y),
                         size = 1) +
      ggplot2::xlab(xlab) +
      ggplot2::ylab(ylab) +
      ggplot2::geom_rug(data = d,
                        mapping = ggplot2::aes(x = x, y = y),
                        sides = "b",
                        alpha = 0.5)
    factorPoints <- NULL
  }


  if (!is.null(conf_min)) {
    d <- cbind(d, conf_lower = conf_min, conf_upper = conf_max)
    if (is.factor(xVar)) {
      d_factor <- unique(d)
      confidenceBounds <- ggplot2::geom_errorbar(data = d_factor,
                                                 ggplot2::aes(x = x, y = y, ymin = conf_lower, ymax = conf_upper),
                                                 linetype = "solid",
                                                 width = 0.1,
                                                 size = 1)
    } else {
      confidenceBounds <- ggplot2::geom_ribbon(mapping = ggplot2::aes(x = x, ymin = conf_lower, ymax = conf_upper),
                                               alpha = .1,
                                               data = d,
                                               size = 1)
    }

  } else {
    confidenceBounds = NULL
  }

  if (!is.null(pred_min)) {
    d <- cbind(d, pred_lower = pred_min, pred_upper = pred_max)
    if (is.factor(xVar)) {
      d_factor <- unique(d)
      predictionBound1 <- ggplot2::geom_errorbar(data = d_factor,
                                                 ggplot2::aes(x = x, y = y, ymin = pred_lower, ymax = pred_upper),
                                                 linetype = "dashed",
                                                 width = 0.1,
                                                 size = 1)
      predictionBound2 <- NULL

    } else {
      predictionBound1 <- ggplot2::geom_line(
        mapping = ggplot2::aes(x = x, y = pred_lower),
        #color = "red",
        linetype = "dashed",
        data = d,
        size = 1)

      predictionBound2 <-
        ggplot2::geom_line(
          mapping = ggplot2::aes(x = x, y = pred_upper),
          #color = "red",
          linetype = "dashed",
          data = d,
          size = 1)
    }
  } else {
    predictionBounds <- predictionBound1 <- predictionBound2 <- NULL
  }

  finalMarginalPlot <- basicMarginalPlot +
    confidenceBounds +
    predictionBound1 +
    predictionBound2 +
    factorPoints +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(axis.title.cex = 1.2)

  return(finalMarginalPlot)
}
