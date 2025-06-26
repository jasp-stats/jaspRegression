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

RegressionLogisticInternal <- function(jaspResults, dataset = NULL, options, ...) {
  ready <- options$dependent != "" #&& options$weights != ""
  if(ready) {
    dataset <- .reglogisticReadData(dataset, options)
    options <- .encodeModelTerms(options, dataset)
    .reglogisticCheckErrors(dataset, options)
  }
  # Output tables
  .reglogisticComputeModel(            jaspResults, dataset, options, ready)
  .reglogisticModelSummaryTable(       jaspResults, dataset, options, ready)
  .reglogisticEstimatesTable(          jaspResults, dataset, options, ready)
  .reglogisticEstimatesTableBootstrap( jaspResults, dataset, options, ready)
  .reglogisticMulticolliTable(         jaspResults, dataset, options, ready)
  .reglogisticFactorDescriptivesTable( jaspResults, dataset, options, ready)

  finalModel <- jaspResults[["glmRes"]][["object"]][[length(jaspResults[["glmRes"]][["object"]])]]
  if (options$residualCasewiseDiagnostic) {
    .glmInfluenceTable(jaspResults, finalModel, dataset, options, ready, position = 5)
  }

  .reglogisticConfusionMatrixTable(    jaspResults, dataset, options, ready)
  .reglogisticPerformanceMetricsTable( jaspResults, dataset, options, ready)

  # Output plots
  .reglogisticEstimatesPlot(              jaspResults, dataset, options, ready)
  .reglogisticPredictedResidualsPlot(     jaspResults, dataset, options, ready)
  .reglogisticPredictorResidualsPlot(     jaspResults, dataset, options, ready)
  .reglogisticSquaredPearsonResidualsPlot(jaspResults, dataset, options, ready)
  .reglogisticIndependentPredictedPlot(   jaspResults, dataset, options, ready)
  .reglogisticPerformancePlot(            jaspResults, dataset, options, ready)

  if (options[["residualsSavedToData"]] && options[["residualsSavedToDataColumn"]] != "" && is.null(jaspResults[["residualsSavedToDataColumn"]]))
    .regressionExportResiduals(jaspResults, finalModel, dataset, options)

  if (options[["predictionsSavedToData"]] && options[["predictionsSavedToDataColumn"]] != "" && is.null(jaspResults[["predictionsSavedToDataColumn"]]))
    .regressionExportPredictions(jaspResults, finalModel, dataset, options)

  return()
}

# Preprocessing functions
.reglogisticReadData <- function(dataset, options) {
  numericVars <- unlist(c(options$covariates, options$weights))
  numericVars <- numericVars[numericVars != ""]
  factorVars  <- unlist(c(options$dependent, options$factors))
  factorVars  <- factorVars[factorVars != ""]

  return(
    excludeNaListwise(dataset, columns = c(numericVars, factorVars, recursive = TRUE))
  )
}

.reglogisticCheckErrors <- function(dataset, options){
  .hasErrors(dataset,
             type = "factorLevels",
             factorLevels.target  = options$dependent,
             factorLevels.amount  = '!= 2',
             exitAnalysisIfErrors = TRUE)
  if (options$weights != "")
    .hasErrors(dataset,
               type = "limits",
               limits.target = options$weights,
               limits.min = 0,
               limits.max = Inf,
               exitAnalysisIfErrors = TRUE)
  if (length(options$covariates) != 0)
    .hasErrors(dataset,
               type = c("observations", "infinity", "variance", "varCovData"),
               all.target = options$covariates,
               observations.amount  = "< 2",
               exitAnalysisIfErrors = TRUE)
  if (length(options$factors) != 0)
    .hasErrors(dataset,
               type = "factorLevels",
               factorLevels.target  = options$factors,
               factorLevels.amount  = '< 2',
               exitAnalysisIfErrors = TRUE)
}

# Performance Diagnostics Container
.reglogisticPerfDiagContainer <- function(jaspResults) {
  if (is.null(jaspResults[["perfDiag"]])) {
    container <- createJaspContainer(gettext("Performance Diagnostics"))
    jaspResults[["perfDiag"]] <- container
  }
}

# Tables
.reglogisticModelSummaryTable <- function(jaspResults, dataset,
                                          options, ready) {
  if(!is.null(jaspResults[["modelSummary"]]))
    return()

  if(options[['dependent']] == "")
    modelSummary <- createJaspTable(gettext("Model Summary"))
  else
    modelSummary <- createJaspTable(gettextf("Model Summary - %s", options[['dependent']]))

  dependList <- c("dependent", "method", "modelTerms", "interceptTerm")
  modelSummary$dependOn(dependList)
  modelSummary$position <- 1
  modelSummary$showSpecifiedColumnsOnly <- TRUE

  modelSummary$addColumnInfo(name = "mod", title = gettext("Model"),    type = "string")
  modelSummary$addColumnInfo(name = "dev", title = gettext("Deviance"), type = "number")
  modelSummary$addColumnInfo(name = "aic", title = gettext("AIC"),      type = "number", format="dp:3")
  modelSummary$addColumnInfo(name = "bic", title = gettext("BIC"),      type = "number", format="dp:3")
  modelSummary$addColumnInfo(name = "dof", title = gettext("df"),       type = "integer")
  modelSummary$addColumnInfo(name = "chi", title = "\u0394\u03A7\u00B2",type = "number", format="dp:3")
  modelSummary$addColumnInfo(name = "pvl", title = gettext("p"),        type = "pvalue")
  modelSummary$addColumnInfo(name = "fad", title = gettextf("McFadden R%s","\u00B2"),    type = "number")
  modelSummary$addColumnInfo(name = "nag", title = gettextf("Nagelkerke R%s","\u00B2"),  type = "number")
  modelSummary$addColumnInfo(name = "tju", title = gettextf("Tjur R%s","\u00B2"),        type = "number")
  modelSummary$addColumnInfo(name = "cas", title = gettextf("Cox & Snell R%s","\u00B2"), type = "number")

  jaspResults[["modelSummary"]] <- modelSummary
  res <- try(.reglogisticModelSummaryFill(jaspResults, dataset, options, ready))

  .reglogisticSetError(res, modelSummary)
}

.reglogisticEstimatesTable <- function(jaspResults, dataset, options, ready) {
  if(!options$coefficientEstimate || !is.null(jaspResults[["estimatesTable"]]))
    return()

  estimatesTable <- createJaspTable(gettext("Coefficients"))
  estimatesTable$dependOn(optionsFromObject   = jaspResults[["modelSummary"]],
                          options             = c("coefficientEstimate", "coefficientStandardized",
                                                  "oddsRatio", "coefficientCi", "robustSe",
                                                  "coefficientCiLevel", "coefficientCiAsOddsRatio",
                                                  "robustSEopt", "vovkSellke"))
  estimatesTable$position <- 2
  estimatesTable$showSpecifiedColumnsOnly <- TRUE

  tmp <- .reglogisticEstimatesInfo(options)
  ciTitle    <- tmp[["ciTitle"]]
  seTitle    <- tmp[["seTitle"]]
  paramtitle <- tmp[["paramtitle"]]

  estimatesTable$addColumnInfo(name = "model", title = gettext("Model"), type = "string", combine = TRUE)
  estimatesTable$addColumnInfo(name = "param",   title = paramtitle, type = "string")
  estimatesTable$addColumnInfo(name = "est",     title = gettext("Estimate"), type = "number", format="dp:3")
  estimatesTable$addColumnInfo(name = "se",      title = seTitle, type = "number", format="dp:3")
  if(options$coefficientStandardized)
    estimatesTable$addColumnInfo(name = "std",   title = gettextf("Standardized%s", "\u207A"), type = "number", format="dp:3")
  if(options$oddsRatio)
    estimatesTable$addColumnInfo(name = "or",    title = gettext("Odds Ratio"), type = "number")
  estimatesTable$addColumnInfo(name = "zval",    title = gettext("z"), type = "number")
  estimatesTable$addColumnInfo(name = "waldsta", title = gettext("Wald Statistic"), type = "number", overtitle = "Wald Test")
  estimatesTable$addColumnInfo(name = "walddf",  title = gettext("df"), type = "integer", overtitle = "Wald Test")
  estimatesTable$addColumnInfo(name = "pval",    title = gettext("p"), type = "pvalue", overtitle = "Wald Test")

  if(options$vovkSellke)
    .reglogisticVovkSellke(estimatesTable, options)

  if(options$coefficientCi) {
    estimatesTable$addColumnInfo(name = "cilo", title = gettext("Lower bound"), type = "number", format="dp:3", overtitle = ciTitle)
    estimatesTable$addColumnInfo(name = "ciup", title = gettext("Upper bound"), type = "number", format="dp:3", overtitle = ciTitle)
  }

  if (options$coefficientStandardized)
    estimatesTable$addFootnote(gettext("Standardized estimates represent estimates where the continuous predictors are standardized (X-standardization)."), symbol = "\u207A")

  jaspResults[["estimatesTable"]] <- estimatesTable

  if(!ready) return()
  res <- try(.reglogisticEstimatesFill(jaspResults, dataset, options))

  .reglogisticSetError(res, estimatesTable)
}

.reglogisticEstimatesTableBootstrap <- function(jaspResults, dataset,
                                                options, ready) {
  if(!options$coefficientBootstrap ||
     !is.null(jaspResults[["estimatesTableBootstrapping"]]))
    return()

  estimatesTableBootstrap <- createJaspTable(gettext("Bootstrap Coefficients"))
  estimatesTableBootstrap$dependOn(optionsFromObject   = jaspResults[["modelSummary"]],
                                   options             = c("coefficientBootstrap",
                                                           "coefficientBootstrapSamples",
                                                           "coefficientCi", "coefficientCiAsOddsRatio", "coefficientStandardized", "oddsRatio", "coefficientCiLevel",
                                                           "robustSe"))
  estimatesTableBootstrap$position <- 3
  estimatesTableBootstrap$showSpecifiedColumnsOnly <- TRUE

  tmp <- .reglogisticEstimatesInfo(options, addBCA = TRUE)
  ciTitle    <- tmp[["ciTitle"]]
  seTitle    <- tmp[["seTitle"]]
  paramtitle <- tmp[["paramtitle"]]

  if(options$method != "enter")
    estimatesTableBootstrap$addColumnInfo(name = "model", title = gettext("Model"),          type = "string", combine = TRUE)
  estimatesTableBootstrap$addColumnInfo(name = "param",   title = paramtitle,                type = "string")
  estimatesTableBootstrap$addColumnInfo(name = "est",     title = gettext("Estimate"),       type = "number", format="dp:3")
  estimatesTableBootstrap$addColumnInfo(name = "bias",    title = gettext("Bias"),           type = "number", format="dp:3")
  estimatesTableBootstrap$addColumnInfo(name = "se",      title = seTitle,                   type = "number", format="dp:3")
  if(options$coefficientStandardized) {
    estimatesTableBootstrap$addColumnInfo(name = "std",   title = gettextf("Standardized%s", "\u207A"), type = "number", format="dp:3")
    estimatesTableBootstrap$addFootnote(gettext("Standardized estimates represent estimates where the continuous predictors are standardized (X-standardization)."), symbol = "\u207A")
  }
  if(options$oddsRatio)
    estimatesTableBootstrap$addColumnInfo(name = "or",    title = gettext("Odds Ratio"), type = "number")
  if (options$coefficientCi) {
    estimatesTableBootstrap$addColumnInfo(name = "cilo",    title = gettext("Lower bound"),    type = "number", format="dp:3", overtitle = ciTitle)
    estimatesTableBootstrap$addColumnInfo(name = "ciup",    title = gettext("Upper bound"),    type = "number", format="dp:3", overtitle = ciTitle)
    estimatesTableBootstrap$addFootnote(gettext("Bias corrected accelerated."), symbol = "\u002A")
  }

  jaspResults[["estimatesTableBootstrapping"]] <- estimatesTableBootstrap

  if(!ready) return()
  res <- try(.reglogisticEstimatesBootstrapFill(jaspResults, dataset, options))

  .reglogisticSetError(res, estimatesTableBootstrap)

  if (options$robustSe)
    estimatesTableBootstrap$addFootnote(gettext("Coefficient estimate and robust standard error are based on the median of the bootstrap distribution."))
  else
    estimatesTableBootstrap$addFootnote(gettext("Coefficient estimate is based on the median of the bootstrap distribution."))
}

.reglogisticMulticolliTable <- function(jaspResults, dataset, options, ready) {
  if(!options$multicollinearity || !is.null(jaspResults[["multicolliTable"]]))
    return()

  multicolliTable <- createJaspTable(gettext("Multicollinearity Diagnostics"))
  multicolliTable$dependOn(optionsFromObject   = jaspResults[["modelSummary"]],
                           options             = "multicollinearity")
  multicolliTable$position <- 4
  multicolliTable$showSpecifiedColumnsOnly <- TRUE

  multicolliTable$addColumnInfo(name = "var", title = "", type = "string")

  if (ready) {
    glmObj <- jaspResults[["glmRes"]][["object"]]
  } else {
    glmObj <- NULL
  }

  multicolliTable$addColumnInfo(name = "tolerance", title = gettext("Tolerance"), type = "number")
  multicolliTable$addColumnInfo(name = "VIF", title = gettext("VIF"), type = "number")

  jaspResults[["multicolliTable"]] <- multicolliTable

  res <- try(.reglogisticMulticolliTableFill(jaspResults, dataset, options, glmObj, ready))

  .reglogisticSetError(res, multicolliTable)
}

.reglogisticFactorDescriptivesTable <- function(jaspResults, dataset, options, ready){
  if(!options$descriptives ||
     !is.null(jaspResults[["factorDescriptives"]]) || isFALSE(ready))
    return()
  dataset <- .reglogisticReadData(dataset, options)
  factorDescriptives <- createJaspTable(gettext("Factor Descriptives"))
  factorDescriptives$dependOn(c("descriptives", "factors"))
  factorDescriptives$position <- 6
  factorDescriptives$showSpecifiedColumnsOnly <- TRUE
  if (length(options$factors) == 0)
    factorDescriptives$addColumnInfo(name = "Factor", title = gettext("Factor"),
                                     type = "string")
  else {
    for (variable in options$factors) {
      name <- paste("__", variable, sep = "")  # distinguish it in case it's "N"
      factorDescriptives$addColumnInfo(name = name, type = "string",
                                       title = variable, combine = TRUE)
    }
  }
  factorDescriptives$addColumnInfo(name = "N", title=gettext("N"), type = "integer")

  jaspResults[["factorDescriptives"]] <- factorDescriptives

  res <- try(.reglogisticFactorDescriptivesFill(jaspResults, dataset, options))

  .reglogisticSetError(res, factorDescriptives)
}

.reglogisticConfusionMatrixTable <- function(jaspResults, dataset, options, ready) {
  .reglogisticPerfDiagContainer(jaspResults)
  container <- jaspResults[["perfDiag"]]
  if(!options$confusionMatrix ||
     !is.null(container[["confusionMatrix"]]))
    return()
  confusionMatrix <- createJaspTable(gettext("Confusion matrix"))
  confusionMatrix$dependOn(optionsFromObject = jaspResults[["modelSummary"]],
                           options           = c("confusionMatrix"))
  confusionMatrix$position <- 7
  confusionMatrix$showSpecifiedColumnsOnly <- TRUE

  confusionMatrix$addColumnInfo(name = "obs", title = gettext("Observed"), type = "string")
  if (ready) {
    # Compute/Get Model
    glmObj <- jaspResults[["glmRes"]][["object"]]
    #modelTerms <- c()
    #for(i in seq_along(options$modelTerms))
    #  modelTerms <- c(modelTerms, options$modelTerms[[i]]$components)
    #levs <- levels(dataset[[.v(modelTerms)]])
    mObj <- glmObj[[length(glmObj)]]
    levs <- levels(mObj[["model"]][,1])
  } else {
    levs   <- c(0,1)
    glmObj <- NULL
  }

  .confusionMatAddColInfo(confusionMatrix, levs, "integer")
  confusionMatrix$addColumnInfo(name = "perCorrect", title = gettextf("%% Correct"), type = "number")

  container[["confusionMatrix"]] <- confusionMatrix

  res <- try(.reglogisticConfusionMatrixFill(container, dataset, options, glmObj, ready))

  .reglogisticSetError(res, confusionMatrix)
}

.reglogisticPerformanceMetricsTable <- function(jaspResults, dataset, options, ready){
  .reglogisticPerfDiagContainer(jaspResults)
  container <- jaspResults[["perfDiag"]]
  if(!is.null(container[["performanceMetrics"]]))
    return()
  performList <- c("accuracy", "auc", "sensitivity", "specificity", "precision", "fMeasure", "brierScore", "hMeasure")
  if(!any(unlist(options[performList])))
    return()
  performanceMetrics <- createJaspTable(gettext("Performance metrics"))
  performanceMetrics$dependOn(optionsFromObject = jaspResults[["modelSummary"]],
                              options           = performList)
  performanceMetrics$position <- 8
  performanceMetrics$showSpecifiedColumnsOnly <- TRUE

  performanceMetrics$addColumnInfo(name = "met", title = "", type = "string")
  performanceMetrics$addColumnInfo(name = "val", title = gettext("Value"), type = "number")

  container[["performanceMetrics"]] <- performanceMetrics

  res <- try(.reglogisticPerformanceMetricsFill(jaspResults, container, dataset, options, ready))

  .reglogisticSetError(res, performanceMetrics)
}

#Table Filling
.reglogisticModelSummaryFill <- function(jaspResults, dataset, options, ready) {
  if(ready) {
    # Compute/Get Model
    glmObj <- jaspResults[["glmRes"]][["object"]]

      # multiple rows: m1 - mk
      rows <- vector("list", length(glmObj))

      for (midx in seq_along(glmObj)) {

        if (options$method == "enter")
          .linregAddPredictorsInModelFootnote(jaspResults[["modelSummary"]],
                                              options[["modelTerms"]][[midx]][["components"]], midx)
        mObj <- glmObj[[midx]]
        if (midx > 1) {
          if (options$method == "forward" ||
              options$method == "stepwise" ||
              options$method == "enter") {
            fadden <- .mcFadden(mObj, glmObj[[1]])
            nagel  <- .nagelkerke(mObj, glmObj[[1]])
            coxSn  <- .coxSnell(mObj, glmObj[[1]])
          } else {
            fadden <- -1*.mcFadden(glmObj[[1]], mObj)
            nagel  <- -1*.nagelkerke(glmObj[[1]], mObj)
            coxSn  <- -1*.coxSnell(glmObj[[1]], mObj)
          }

          lr <- .lrtest(glmObj[[midx-1]], mObj)
          rows[[midx]] <- list(
            mod = gettextf("M%s", intToUtf8(0x2080 + midx - 1, multiple = FALSE)),
            dev = mObj[["deviance"]],
            aic = mObj[["aic"]],
            bic = .bic(mObj),
            dof = mObj[["df.residual"]],
            chi = lr[["stat"]],
            pvl = lr[["pval"]],
            fad = fadden,
            nag = nagel,
            tju = .tjur(mObj),
            cas = coxSn
          )
        } else {
          rows[[midx]] <- list(
            mod = gettextf("M%s", intToUtf8(0x2080, multiple = FALSE)),
            dev = mObj[["deviance"]],
            aic = mObj[["aic"]],
            bic = .bic(mObj),
            dof = mObj[["df.residual"]],
            chi = NULL,
            pvl = NULL,
            fad = .mcFadden(mObj, mObj),
            nag = .nagelkerke(mObj, mObj),
            tju = .tjur(mObj),
            cas = .coxSnell(mObj, mObj)
          )
        }

    }
  } else {
    rows <- list(
      list(mod = "H\u2080", dev = ".", fad = NULL,
           nag = NULL, tju = NULL, cas = NULL, aic = "."),
      list(mod = "H\u2081", dev = ".", fad = ".",
           nag = ".", tju = ".", cas = ".", aic = ".")
    )
  }
  jaspResults[["modelSummary"]]$addRows(rows)
}

.reglogisticEstimatesFill <- function(jaspResults, dataset, options) {
  # Compute/Get Model
  glmObj <- jaspResults[["glmRes"]][["object"]]
  alpha  <- qnorm(1 - (1 - options$coefficientCiLevel) / 2)


  for (midx in seq_along(glmObj)) {
    mObj <- glmObj[[midx]]
    s    <- summary(mObj)[["coefficients"]]
    rn   <- rownames(s)
    rn[which(rn == "(Intercept)")] <- gettext("(Intercept)")
    beta <- .stdEst(mObj, type = "X") # stand. X continuous vars

    # Confidence intervals on the odds ratio scale
    if (options$coefficientCiAsOddsRatio)
      expon <- function(x) exp(x)
    else
      expon <- function(x) x


    if (length(rn) == 1) {
      s <- unname(s)
      if (options$robustSe) {
        s[2] <- unname(.glmRobustSE(mObj)) # new se
        s[3] <- s[1]/s[2] # new z
        s[4] <- 2*pnorm(-abs(s[3])) # new p
      }
      waldtest <- .waldTest(mObj, term = 1)

      jaspResults[["estimatesTable"]]$addRows(list(
        model   = gettextf("M%s", intToUtf8(0x2080 + midx - 1, multiple = FALSE)),
        param   = .formatTerm(rn, mObj),
        est     = s[1],
        se      = s[2],
        std     = as.numeric(beta),
        or      = exp(s[1]),
        zval    = s[3],
        pval    = s[4],
        waldsta = as.numeric(waldtest),
        walddf  = as.numeric(1),
        vsmpr   = VovkSellkeMPR(s[4]),
        cilo    = expon(s[1] - alpha * s[2]),
        ciup    = expon(s[1] + alpha * s[2]),
        .isNewGroup = TRUE
      ))
    } else {
      if (options$robustSe) {
        s[,2] <- unname(.glmRobustSE(mObj)) # new se
        s[,3] <- s[,1]/s[,2] # new z
        s[,4] <- 2*pnorm(-abs(s[,3])) # new p
      }
      for (i in seq_along(rn)) {

        waldtest <- .waldTest(mObj, term = i)
        jaspResults[["estimatesTable"]]$addRows(list(
          model   = gettextf("M%s", intToUtf8(0x2080 + midx - 1, multiple = FALSE)),
          param   = .formatTerm(rn[i], mObj),
          est     = s[i,1],
          se      = s[i,2],
          std     = as.numeric(beta[i]),
          or      = exp(s[i,1]),
          zval    = s[i,3],
          pval    = s[i,4],
          waldsta = as.numeric(waldtest),
          walddf  = as.numeric(1),
          vsmpr   = VovkSellkeMPR(s[i,4]),
          cilo    = expon(s[i,1] - alpha * s[i,2]),
          ciup    = expon(s[i,1] + alpha * s[i,2]),
          .isNewGroup = (i == 1)
        ))
      }
    }
  }

  predVar   <- options[["dependent"]]
  predLevel <- levels(dataset[[predVar]])[2]

  jaspResults[["estimatesTable"]]$addFootnote(gettextf("%1$s level '%2$s' coded as class 1.", predVar, predLevel))
}

.reglogisticEstimatesBootstrapFill <- function(jaspResults, dataset, options){
  # Compute/Get Model
  glmObj     <- jaspResults[["glmRes"]][["object"]]
  ci.fails   <- FALSE

  if (is.null(jaspResults[["bootstrapResults"]])) {
    bootstrapResults <- list()
  } else {
    bootstrapResults <- jaspResults[["bootstrapResults"]]$object
  }
  if (!is.null(glmObj)) {

    expon <- if (options$coefficientCiAsOddsRatio) function(x) exp(x) else identity

    startProgressbar(options$coefficientBootstrapSamples *
                     length(glmObj))

    for (i in 1:length(glmObj)) {
      if (i != 2)
        next

        rn <- rownames(summary(glmObj[[i]])[["coefficients"]])
        rn[which(rn == "(Intercept)")] <- gettext("(Intercept)")
        bootname <- paste(rn, collapse = "-")

        if (is.null(bootstrapResults[[bootname]])) {

          # vandenman: we compute additional statistics while bootstrapping, but we can't do this using boot
          # so we hack it in there using an environment
          # kucharssim: this is not true, you can boot whatever statistics you want, but ok... the bootstrapping should get some review anyway at some point
          # discussion here: https://github.com/jasp-stats/jasp-desktop/pull/3962#discussion_r394348052
          envir <- new.env()
          envir$idx <- envir$idx_rse <- 0L
          envir$stdEst <- envir$robustSE <-
            matrix(NA, options$coefficientBootstrapSamples, length(coef(glmObj[[i]])))

          .bootstrapping    <- function(data, indices, model.formula, options, envir) {
            progressbarTick()

            d <- data[indices, , drop = FALSE] # allows boot to select sample
            result <- glm(model.formula, family = "binomial", data = d)

            if(length(coef(result)) != length(coef(glmObj[[i]]))) return(rep(NA, length(coef(glmObj[[i]]))))
            if (envir$idx == 0L) {
              # boot::boot does one test run before doing all runs (which it ignores in the results)
              envir$idx     <- 1L
              envir$idx_rse <- 1L
            } else {
              resultStd <- try(unname(.stdEst(result, type = "X")))
              if (!isTryError(resultStd)) {
                envir$stdEst[envir$idx, ] <- resultStd
                envir$idx <- envir$idx + 1L
              }
              robustSE <- try(unname(.glmRobustSE(result)))
              if (!isTryError(robustSE)) {
                envir$robustSE[envir$idx_rse, ] <- robustSE
                envir$idx_rse <- envir$idx_rse + 1L
              }
            }
            return(coef(result))
          }

          bootstrap.summary <- try(boot::boot(data = dataset,
                                              statistic = .bootstrapping,
                                              R = options$coefficientBootstrapSamples,
                                              model.formula = formula(glmObj[[i]]),
                                              options = options,
                                              envir = envir),
                                   silent = TRUE)

          bootstrap.summary$stdEst   <- envir$stdEst
          bootstrap.summary$robustSE <- envir$robustSE
          bootstrapResults[[bootname]] <- bootstrap.summary
          try(rm(envir, envir = parent.env(envir)), silent = TRUE)

        } else {
          bootstrap.summary <- bootstrapResults[[bootname]]
        }

      bootstrap.coef <- matrixStats::colMedians(bootstrap.summary$t, na.rm = TRUE)
      bootstrap.std  <- matrixStats::colMedians(bootstrap.summary$stdEst, na.rm = TRUE)
      bootstrap.bias <- colMeans(bootstrap.summary$t, na.rm = TRUE) - bootstrap.summary$t0
      bootstrap.or   <- matrixStats::colMedians(exp(bootstrap.summary$t), na.rm = TRUE)
      if (options$robustSe)
        bootstrap.se <- matrixStats::colMedians(bootstrap.summary$robustSE, na.rm = TRUE)
      else
        bootstrap.se <- matrixStats::colSds(as.matrix(bootstrap.summary$t), na.rm = TRUE)

      jaspResults[['estimatesTableBootstrapping']]$addFootnote(gettextf("Bootstrapping based on %i successful replicates.", sum(complete.cases(bootstrap.summary$t))))
      for (j in seq_along(rn)) {

        row <- list(
          model = as.character(i),
          param = .formatTerm(rn[j], glmObj[[i]]),
          est   = as.numeric(bootstrap.coef[j]),
          bias  = as.numeric(bootstrap.bias[j]),
          se    = as.numeric(bootstrap.se[j]),
          .isNewGroup = as.logical(j == 1)
        )

        if (options$coefficientCi) {
          result.bootstrap.ci <- try(boot::boot.ci(bootstrap.summary, type = "bca", conf = options$coefficientCiLevel, index=j))
          if(!isTryError(result.bootstrap.ci))
            bootstrap.ci <- result.bootstrap.ci
          else if(identical(attr(result.bootstrap.ci, "condition")$message, "estimated adjustment 'a' is NA")){
            # NOTE: the if statement above doesn't work if the package uses gettext and translates error messages.
            ci.fails <- TRUE
            bootstrap.ci <- list(bca = rep(NA, 5))
          } else
            bootstrap.ci <- result.bootstrap.ci

          if(ci.fails)
            jaspResults[['estimatesTableBootstrapping']]$addFootnote(gettext("Some confidence intervals could not be computed.\nPossibly too few bootstrap replicates."))

          row[["cilo"]] <- expon(as.numeric(bootstrap.ci$bca[4]))
          row[["ciup"]] <- expon(as.numeric(bootstrap.ci$bca[5]))
        }

        row[["or"]]  <- bootstrap.or[j]
        row[["std"]] <- bootstrap.std[j]

        jaspResults[["estimatesTableBootstrapping"]]$addRows(row)
      }
    }
    bootstrapResultsState <- createJaspState(bootstrapResults)
    bootstrapResultsState$dependOn(optionsFromObject   = jaspResults[["modelSummary"]],
                                   options             = c("coefficientBootstrap", "coefficientBootstrapSamples"))
    jaspResults[["bootstrapResults"]] <- bootstrapResultsState
  } else {
    return()
  }
}

.reglogisticFactorDescriptivesFill <- function(jaspResults, dataset, options){

  if (length(options$factors) > 0) {
    lvls    <- list()
    factors <- list()

    for (variable in options$factors) {
      factor <- dataset[[ .v(variable) ]]
      factors[[length(factors)+1]] <- factor
      lvls[[ variable ]] <- levels(factor)
    }

    cases <- rev(expand.grid(rev(lvls)))
    namez <- unlist(options$factors)
    columnNames <- paste("__", namez, sep="")

    if (length(options$factors) > 0) {
      for (i in 1:dim(cases)[1]) {
        row <- list()
        for (j in 1:dim(cases)[2])
          row[[ columnNames[[j]] ]] <- as.character(cases[i, j])

        # eval parse? Is this really the best way to do this?
        sub   <- eval(parse(text=paste("dataset$", .v(namez), " == \"", row, "\"", sep="", collapse = " & ")))
        dat   <- subset(dataset, sub)[[1]]
        N     <- length(dat)

        row$N <- N
        row[[".isNewGroup"]]   <- cases[i,dim(cases)[2]] == lvls[[ dim(cases)[2] ]][1]

        jaspResults[["factorDescriptives"]]$addRows(row)
      }
    }
  } else
    jaspResults[["factorDescriptives"]]$addRows(list(Factor = ".", N = "."))

}

.reglogisticConfusionMatrixFill <- function(container, dataset, options, glmObj, ready) {
  if (ready) {
    mObj   <- glmObj[[length(glmObj)]]
    levs   <- levels(mObj[["model"]][,1])
    m <- .confusionMatrix(mObj, cutoff = 0.5)[["matrix"]]
    n = sum(m)
    rowTotal1 <- rowSums(m)[[1]]
    rowTotal2 <- rowSums(m)[[2]]
    rowPerCorrect1 <- m[1,1]/rowTotal1*100
    rowPerCorrect2 <- m[2,2]/rowTotal2*100
    accuracy <- (m[1,1]+m[2,2])/n*100

    container[["confusionMatrix"]]$addRows(list(
      list(obs = levs[1], pred0 = m[1,1], pred1 = m[1,2], perCorrect = rowPerCorrect1),
      list(obs = levs[2], pred0 = m[2,1], pred1 = m[2,2], perCorrect = rowPerCorrect2),
      list(obs = "Overall % Correct", pred0 = "", pred1 = "", perCorrect = accuracy)
    ))

    message <- "The cut-off value is set to 0.5"
    container[["confusionMatrix"]]$addFootnote(message)
  } else
    container[["confusionMatrix"]]$addRows(list(
      list(obs = "0", pred0 = ".", pred1 = ".", total = "."),
      list(obs = "1", pred0 = ".", pred1 = ".", total = "."),
      list(obs = "Overall % Correct", pred0 = ".", pred1 = ".", total = ".")
    ))
}

.reglogisticMulticolliTableFill <- function(jaspResults, dataset, options, glmObj, ready) {
  if (ready && !is.null(glmObj)) {
    mObj          <- glmObj[[length(glmObj)]]
    vif_obj       <- .vif.default(mObj)

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
      jaspResults[["multicolliTable"]]$addRows(list(var       = var_names[[i]],
                                                    tolerance = tolerance_vec[[i]],
                                                    VIF       = vif_vec[[i]]))
    }

  } else
    jaspResults[["multicolliTable"]]$addRows(
      list(var = ".", tolerance = ".", VIF = "."))
}

.reglogisticPerformanceMetricsFill <- function(jaspResults, container, dataset, options, ready){
  if(ready) {
    # Compute/Get Model
    glmObj <- jaspResults[["glmRes"]][["object"]]
    mObj   <- glmObj[[length(glmObj)]]
    m <- .confusionMatrix(mObj, cutoff = 0.5)[["matrix"]]
    n = sum(m)
    accuracy <- (m[1,1]+m[2,2])/n

    metrics <- .confusionMatrix(mObj, cutoff = 0.5)[["metrics"]]
    rows <- list(
      list(met = "Accuracy",    val = accuracy),
      list(met = "AUC",         val = metrics[["AUC"]]),
      list(met = "Sensitivity", val = metrics[["Sens"]]),
      list(met = "Specificity", val = metrics[["Spec"]]),
      list(met = "Precision",   val = metrics[["Precision"]]),
      list(met = "F-measure",   val = metrics[["F"]]),
      list(met = "Brier score", val = metrics[["Brier"]]),
      list(met = "H-measure",   val = metrics[["H"]])
    )
  } else
    rows <- list(
      list(met = "Accuracy",    val = "."),
      list(met = "AUC",         val = "."),
      list(met = "Sensitivity", val = "."),
      list(met = "Specificity", val = "."),
      list(met = "Precision",   val = "."),
      list(met = "F-measure",   val = "."),
      list(met = "Brier score", val = "."),
      list(met = "H-measure",   val = ".")
    )

  # determine which scores we need
  scrNeed  <- with(options, c(accuracy, auc, sensitivity, specificity, precision, fMeasure, brierScore, hMeasure))
  rows     <- rows[scrNeed]
  container[["performanceMetrics"]]$addRows(rows)
}

# Plots
.reglogisticDiagnosticPlotContainer <- function(jaspResults, options){
  if(!options$residualVsFittedPlot && !options$residualVsPredictorPlot &&
     !options$squaredPearsonResidualVsFittedPlot && !options$independentVsPredictedPlot)
    return()
  if (is.null(jaspResults[["diagnosticPlots"]])) {
    container <- createJaspContainer(gettext("Diagnostic plots"))
    container$dependOn(optionsFromObject = jaspResults[["modelSummary"]],
                       options           = c("residualType"))
    jaspResults[["diagnosticPlots"]] <- container
  }
}

.reglogisticEstimatesPlot <- function(jaspResults, dataset, options, ready){
  if(!options$conditionalEstimatePlot)
    return()
  jaspResults[["estimatesPlot"]] <- createJaspContainer(gettext("Estimates plots"))
  jaspResults[["estimatesPlot"]]$dependOn(optionsFromObject = jaspResults[["modelSummary"]])
  container <- jaspResults[["estimatesPlot"]]

  if(!ready){
    container[["placeholder"]] <- createJaspPlot(width = 320, height = 320, dependencies = "conditionalEstimatePlot")
    return()
  }
  # Compute/Get Model
  glmObj <- jaspResults[["glmRes"]][["object"]]
  mObj   <- glmObj[[length(glmObj)]]
  predictors <- character(0)

  if (is.null(options$modelTerms[[length(glmObj)]][["components"]])) {
    return()
  }
  mComponents <- options$modelTerms[[length(glmObj)]][["components"]]
  predictors <- unlist(sapply(mComponents, function(x) if(length(x) == 1) x))

  if (length(predictors) > 0 && !is.null(glmObj)) {
    # plot only predictors selected in the final model
    predictors <- predictors[.v(predictors) %in% attr(mObj[["terms"]],
                                                      "term.labels")]
    for(pred in predictors) {
      estimatesPlot <- createJaspPlot(title = pred, width = 480, height = 320)
      estimatesPlot$dependOn(options = c("conditionalEstimatePlot", "conditionalEstimatePlotCi", "conditionalEstimatePlotPoints"))
      p <- try(.reglogisticEstimatesPlotFill(options, mObj, pred))
      if(isTryError(p))
        estimatesPlot$setError(.extractErrorMessage(p))
      else
        estimatesPlot$plotObject <- p
      container[[pred]] <- estimatesPlot
    }
  }
  return()
}

.reglogisticPredictedResidualsPlot <- function(jaspResults, dataset, options, ready){
  if(!options$residualVsFittedPlot)
    return()
  .reglogisticDiagnosticPlotContainer(jaspResults, options)
  container <- jaspResults[["diagnosticPlots"]]

  title <- gettext("Predicted - residuals plot")
  predictedPlot <- createJaspPlot(title = title, width = 480, height = 320)
  predictedPlot$dependOn("residualVsFittedPlot")

  if(ready){
    # Compute/Get Model
    glmObj <- jaspResults[["glmRes"]][["object"]]
    mObj   <- glmObj[[length(glmObj)]]
    p      <- try(.reglogisticResidPlotFill(options, mObj))
    if(isTryError(p))
      predictedPlot$setError(.extractErrorMessage(p))
    else
      predictedPlot$plotObject <- p
  }
  container[["predicted"]] <- predictedPlot
  return()
}

.reglogisticPredictorResidualsPlot <- function(jaspResults, dataset, options, ready){
  if(!options$residualVsPredictorPlot)
    return()
  .reglogisticDiagnosticPlotContainer(jaspResults, options)
  container    <- jaspResults[["diagnosticPlots"]]
  container[["subContainer"]] <- createJaspContainer(gettext("Predictor - residuals plots"))
   subcontainer <- container[["subContainer"]]
  if(!ready){
    subcontainer[["placeholder"]] <- createJaspPlot(width = 320, height = 320, dependencies = "residualVsPredictorPlot")
    return()
  }
  # Compute/Get final Model
  glmObj <- jaspResults[["glmRes"]][["object"]]
  mObj   <- glmObj[[length(glmObj)]]
  mComponents <- options$modelTerms[[length(glmObj)]][["components"]]
  predictors <- unlist(sapply(mComponents, function(x) if(length(x) == 1) x))

  for(pred in predictors) {
    predictorPlot <- createJaspPlot(title = pred, width = 480, height = 320)
    predictorPlot$dependOn("residualVsPredictorPlot")

    p <- try(.reglogisticResidPlotFill(options, mObj, pred))
    if(isTryError(p))
      predictorPlot$setError(.extractErrorMessage(p))
    else
      predictorPlot$plotObject <- p
    subcontainer[[pred]] <- predictorPlot
  }
  return()
}

.reglogisticSquaredPearsonResidualsPlot <- function(jaspResults, dataset, options, ready){
  if(!options$squaredPearsonResidualVsFittedPlot)
    return()
  .reglogisticDiagnosticPlotContainer(jaspResults, options)
  container <- jaspResults[["diagnosticPlots"]]

  title <- gettext("Squared Pearson residuals plot")
  squaredPearsonPlot <- createJaspPlot(title = title, width = 480, height = 320)
  squaredPearsonPlot$dependOn("squaredPearsonResidualVsFittedPlot")

  if(ready){
    # Compute/Get Model
    glmObj <- jaspResults[["glmRes"]][["object"]]
    mObj   <- glmObj[[length(glmObj)]]
    p      <- try(.reglogisticSquaredPearsonResidualsFill(mObj))
    if(isTryError(p))
      squaredPearsonPlot$setError(.extractErrorMessage(p))
    else
      squaredPearsonPlot$plotObject <- p
  }
  container[["squaredPearson"]] <- squaredPearsonPlot
  return()
}

.reglogisticIndependentPredictedPlot <- function(jaspResults, dataset, options, ready){
  if(!options$independentVsPredictedPlot)
    return()
  .reglogisticDiagnosticPlotContainer(jaspResults, options)
  container    <- jaspResults[["diagnosticPlots"]]
  container[["subContainer"]] <- createJaspContainer(gettext("Independent - predicted plots"))
  subcontainer <- container[["subContainer"]]
  if(!ready){
    subcontainer[["placeholder"]] <- createJaspPlot(width = 480, height = 320, dependencies = c("independentVsPredictedPlot",
                                                                                                "independentVsPredictedPlotIncludeInteractions",
                                                                                                "independentVsPredictedPlotUseLogit"))
    return()
  }

  # Compute/Get final Model
  glmObj <- jaspResults[["glmRes"]][["object"]]
  mObj   <- glmObj[[length(glmObj)]]
  mComponents <- options$modelTerms[[length(glmObj)]][["components"]]

  if (options[["independentVsPredictedPlotIncludeInteractions"]]) {
    predictors <- sapply(mComponents, function(x) if(length(x) < 3) x)
  } else {
    predictors <- sapply(mComponents, function(x) if(length(x) == 1) x)
  }

  predictions <- predict(mObj, type = "response")
  if(options[["independentVsPredictedPlotUseLogit"]]) {
    predictions <- log(predictions / (1 - predictions))
    yName <- "Logit Predicted Probability"
  } else {
    yName <- "Predicted Probability"
  }

  for (pred in predictors) {

    facPredictorIndex <- which(pred %in% options[["factors"]])

    for (i in seq_along(pred)) {

    predictorLogitPlot <- createJaspPlot(title =  paste(c(pred[i], pred[-i]), collapse = " \u273B "), width = 480, height = 320)
    predictorLogitPlot$dependOn(c("independentVsPredictedPlot",
                                  "independentVsPredictedPlotIncludeInteractions",
                                  "independentVsPredictedPlotUseLogit"))

    binContVar <- FALSE
    if (length(pred) == 1) {
      groupVar <- groupName <- NULL
      indepVar <- pred
    } else if (length(facPredictorIndex) == 0) { # no factor variables, so give both versions
      indepVar <- pred[i]
      groupName<- pred[-i]
      groupVar <- dataset[[groupName]]
      binContVar <- length(unique(groupVar)) > 5
      groupVar <- if (binContVar) cut(groupVar, 4) else groupVar
    } else if (length(facPredictorIndex) == 1) { # one factor var, so use that as grouping variable
      indepVar <- pred[-facPredictorIndex]
      groupName<- pred[facPredictorIndex]
      groupVar <- dataset[[groupName]]
    } else { # both are factors, so give both versions
      indepVar <- pred[i]
      groupName<- pred[-i]
      groupVar <- dataset[[groupName]]
    }

    p <- try(jaspGraphs::JASPScatterPlot(as.numeric(dataset[[indepVar]]),
                                         predictions,
                                         group = groupVar,
                                         xName = indepVar,
                                         yName = yName,
                                         addSmooth = TRUE,
                                         addSmoothCI = TRUE,
                                         plotAbove = "none",
                                         plotRight = "none",
                                         showLegend = length(pred) > 1,
                                         legendTitle = if (binContVar) paste0(groupName,"_binned") else groupName,
                                         smoothCIValue = 0.95,
                                         forceLinearSmooth = options[["independentVsPredictedPlotUseLogit"]]))

    if(isTryError(p))
      predictorLogitPlot$setError(.extractErrorMessage(p))
    else {
      if (length(pred) > 1)
        p <- p$subplots$mainPlot #+ ggplot2::theme(legend.position="bottom")
      predictorLogitPlot$plotObject <- p
    }
    subcontainer[[paste0(indepVar, groupName)]] <- predictorLogitPlot

    }
  }

  return()
}

.reglogisticPerformancePlot <- function(jaspResults, dataset, options, ready) {
  if (!options$rocPlot && !options$precisionRecallPlot)
    return()

  jaspResults[["performancePlots"]] <- createJaspContainer(gettext("Performance plots"))
  container <- jaspResults[["performancePlots"]]

  if (options$rocPlot) {

    title <- gettext("ROC plot")
    rocPlot <- createJaspPlot(title = title, width = 480, height = 320)
    rocPlot$dependOn(c("rocPlot", "rocPlotCutoffStep"))

    if (ready) {
      glmObj <- jaspResults[["glmRes"]][["object"]]
      mObj   <- glmObj[[length(glmObj)]]

      cutoffInt <- seq(0, 1, by = options$rocPlotCutoffStep)

      tprate <- numeric(length(cutoffInt))
      fprate <- numeric(length(cutoffInt))

      p      <- try(.reglogisticPerformancePlotFill(mObj, options$rocPlotCutoffLabel, cutoffInt, type = "ROC"))

      if (isTryError(p))
        rocPlot$setError(.extractErrorMessage(p))
      else
        rocPlot$plotObject <- p
    }

    container[["rocPlot"]] <- rocPlot
  }

  if (options$precisionRecallPlot) {

    title <- gettext("PR plot")
    prPlot <- createJaspPlot(title = title, width = 480, height = 320)
    prPlot$dependOn(c("precisionRecallPlot", "precisionRecallPlotCutoffStep"))

    if (ready) {
      glmObj <- jaspResults[["glmRes"]][["object"]]
      mObj   <- glmObj[[length(glmObj)]]

      cutoffInt <- seq(0, 1, by = options$precisionRecallPlotCutoffStep)

      tprate <- numeric(length(cutoffInt))
      fprate <- numeric(length(cutoffInt))

      p      <- try(.reglogisticPerformancePlotFill(mObj, options$precisionRecallPlotCutoffLabel, cutoffInt, type = "PR"))

      if (isTryError(p))
        prPlot$setError(.extractErrorMessage(p))
      else
        prPlot$plotObject <- p
    }

    container[["prPlot"]] <- prPlot
  }
  return()
}

# Plot Filling
.reglogisticEstimatesPlotFill <- function(options, mObj, pred){
  # If user wants raw data points, get them from data
  points <- options$conditionalEstimatePlotPoints
  if (points) {
    mf <- model.frame(mObj)
    factors <- names(mObj[["xlevels"]])
    if (pred %in% factors)
      vd <- as.factor(mObj[["data"]][[pred]])
    else
      vd <- mf[, pred]
    ggdat <- data.frame(x = vd, y = mObj$y)
  }
  # Calculate ribbon & main line
  ribdat <- .glmLogRegRibbon(mObj, pred, options$conditionalEstimatePlotCi)
  # Find predicted level
  predVar   <- as.character(mObj[["terms"]])[2]
  predLevel <- levels(mObj[["data"]][[predVar]])[2]

  # this will become the y-axis title
  ytitle <- substitute(expr = "P("*x~"="~y*")",
                       env = list(x = .unv(predVar), y = predLevel))
  if (attr(ribdat, "factor")) {
    # the variable is a factor, plot points with errorbars
    p <- ggplot2::ggplot(ribdat, ggplot2::aes(x = x, y = y))

    if (points) {
      p <- p + ggplot2::geom_point(data = ggdat, size = 2,
        position = ggplot2::position_jitter(height = 0.01, width = 0.04),
        color = "dark grey", alpha = 0.3)
    }
    p <- p +
      ggplot2::geom_point(data = ribdat,
        mapping = ggplot2::aes(x = x, y = y),
        size = 4
      ) +
      ggplot2::geom_errorbar(
        mapping = ggplot2::aes(x = x, ymin = lo, ymax = hi),
        data = ribdat, width = 0.2
      )

  } else {
    # the variable is continuous, plot curve with error ribbon
    p <- ggplot2::ggplot(ribdat, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_ribbon(data = ribdat,
        mapping = ggplot2::aes(x = x, ymax = hi, ymin = lo),
        fill = "light grey", alpha = 0.5
      ) +
      ggplot2::geom_line(data = ribdat,
        mapping = ggplot2::aes(x = x, y = y),
        size = .75, color = "black"
      )

    if (points) {
      p <- p + ggplot2::geom_point(data = ggdat, size = 2,
        position = ggplot2::position_jitter(height = 0.03, width = 0),
        color = "dark grey", alpha = 0.3)
    }
  }

  # We've got our plot, time for some theming!
  # First, define custom y and x-axes to draw
  custom_y_axis <- function() {
    d <- data.frame(x = -Inf, xend = -Inf, y = 0, yend = 1)
    list(
      ggplot2::geom_segment(data = d,
        ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
        inherit.aes = FALSE, size = 1),
      ggplot2::scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1))
    )
  }

  custom_x_axis <- function(ribdat) {
    l <- NULL
    xdat <- ribdat[["x"]]
    if (attr(ribdat, "factor"))
      l <- list(ggplot2::scale_x_discrete(labels = levels(xdat)))
    else {
      b <- pretty(xdat)
      d <- data.frame(y = -Inf, yend = -Inf, x = min(b), xend = max(b))
      l <- list(
        ggplot2::geom_segment(data = d,
          ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
          inherit.aes = FALSE, size = 1 ),
        ggplot2::scale_x_continuous(breaks = b)
      )
    }
  }

  # then perform the theme and return the ggplot object
  p <- jaspGraphs::themeJasp(p, legend.position = "none")

  p <- p + ggplot2::xlab(pred) + ggplot2::ylab(ytitle) +
    custom_x_axis(ribdat) + custom_y_axis()
  return(p)
}

.reglogisticResidPlotFill <- function(options, glmModel, var = NULL){
  # plots residuals against predicted (var = NULL) or predictor (var = "name")
  type <- options$residualType
  if (!is.null(var))
    ggdat <- data.frame(resid = residuals(glmModel, type = type),
                        x = glmModel[["data"]][[.v(var)]])
  else {
    ggdat <- data.frame(resid = residuals(glmModel, type = type),
                        x = predict(glmModel, type = "response"))
    var <- gettext("Predicted Probability")
  }

  if (class(ggdat[["x"]]) == "factor")
    pos <- ggplot2::position_jitter(width = 0.1)
  else
    pos <- ggplot2::position_jitter(width = 0)

  custom_y_axis <- function(val) {
    d <- data.frame(x = -Inf, xend = -Inf,
                    y = min(pretty(val)), yend = max(pretty(val)))
    list(
      ggplot2::geom_segment(data = d,
        ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
        inherit.aes = FALSE, size = 1),
      ggplot2::scale_y_continuous(breaks = pretty(val))
    )
  }

  custom_x_axis <- function(val) {
    if (class(val) == "factor")
      l <- list(ggplot2::scale_x_discrete(labels = levels(val)))
    else {
      d <- data.frame(y = -Inf, yend = -Inf,
                      x = min(pretty(val)), xend = max(pretty(val)))
      l <- list(
        ggplot2::geom_segment(data = d,
          ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
          inherit.aes = FALSE, size = 1),
        ggplot2::scale_x_continuous(breaks = pretty(val))
      )
    }
    return(l)
  }

  p <- ggplot2::ggplot(data = ggdat,
                       mapping = ggplot2::aes(x = x, y = resid)) +
    ggplot2::geom_point(position = pos, size = 3,
                        colour = "black", fill = "grey", pch = 21)

  p <- p +
    ggplot2::xlab(var) + ggplot2::ylab(gettext("Residuals")) +
    ggplot2::theme_bw() +
    custom_y_axis(ggdat[["resid"]]) + custom_x_axis(ggdat[["x"]])

  p <- jaspGraphs::themeJasp(p, legend.position = "none")

  return(p)
}

.reglogisticSquaredPearsonResidualsFill <- function(glmModel){
  # Squared Pearson residuals plot courtesy of Dan Gillen (UC Irvine)
  plotDat <- data.frame("pres" = residuals(glmModel, type = "pearson")^2,
                        "prob" = predict(glmModel, type = "response"))

  custom_y_axis <- function(ydat) {
    b <- pretty(c(ydat,0))
    d <- data.frame(y = 0, yend = max(b), x = -Inf, xend = -Inf)
    l <- list(ggplot2::geom_segment(data = d,
                                    ggplot2::aes(x = x, y = y, xend = xend,
                                                 yend = yend),
                                    inherit.aes = FALSE, size = 1),
              ggplot2::scale_y_continuous(breaks = b))
  }

  custom_x_axis <- function() {
    d <- data.frame(y = -Inf, yend = -Inf, x = 0, xend = 1)
    list(ggplot2::geom_segment(data = d, ggplot2::aes(x = x, y = y,
                                                      xend = xend,
                                                      yend = yend),
                               inherit.aes = FALSE, size = 1),
         ggplot2::scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)))
  }

  p <- ggplot2::ggplot(mapping = ggplot2::aes(x = prob, y = pres),
                       data = plotDat) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, y = 1, xend = 1, yend = 1),
                          linetype = 3, size = 1, colour = "grey") +
    ggplot2::geom_smooth(se = FALSE, method = "loess", size = 1.2,
                         colour = "darkred") +
    ggplot2::geom_point(size = 3, colour="black", fill = "grey", pch=21) +
    custom_y_axis(plotDat[["pres"]]) + custom_x_axis() +
    ggplot2::labs(x = gettext("Predicted Probability"), y = gettext("Squared Pearson Residual"))

  p <- jaspGraphs::themeJasp(p, legend.position = "none")

  return(p)
}

.reglogisticPerformancePlotFill <- function(glmModel, addCutoffLabels = FALSE, cutoffInt = seq(0, 1, length.out = 5), type = c("ROC", "PR")) {

  if (type == "ROC") {
    tprate <- numeric(length(cutoffInt))
    fprate <- numeric(length(cutoffInt))

    for (step in 1:length(cutoffInt)) {
      h <- .confusionMatrix(glmModel, cutoff = cutoffInt[step])[["metrics"]]
      tprate[step] <- h[["Sens"]]
      fprate[step] <- 1 - h[["Spec"]]
    }

    plotDat <- data.frame(x = fprate, y = tprate, z = cutoffInt)
    plotDat <- plotDat[order(plotDat$y), ]

  } else {
    precision <- numeric(length(cutoffInt))
    recall <- numeric(length(cutoffInt))

    for (step in 1:length(cutoffInt)) {
      h <- .confusionMatrix(glmModel, cutoff = cutoffInt[step])[["metrics"]]
      precision[step] <- h[["Precision"]]
      recall[step]    <- h[["Sens"]]
    }

    plotDat <- data.frame(x = recall, y = precision, z = cutoffInt)
  }



  p <- ggplot2::ggplot(data = plotDat, mapping = ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(mapping = ggplot2::aes(x = x, y = y),
                        size = 3, shape = 21, fill = "gray") +
    ggplot2::lims(x = c(0, 1), y = c(0, 1)) +
    ggplot2::scale_color_gradient(breaks = c(0, 0.5, 1))

  p <- switch(type,
              ROC = p + ggplot2::labs(x = gettext("False positive rate"),
                                      y = gettext("True positive rate")),
              PR  = p + ggplot2::labs(x = gettext("True positive rate"),
                                      y = gettext("Positive predictive value")))

  if(isTRUE(addCutoffLabels)) p <- p +
    ggrepel::geom_text_repel(mapping = ggplot2::aes(label = gettext(as.character(z))),
                             nudge_y = 0.02,
                             point.padding = 0.5,
                             size = 6)

  p <- jaspGraphs::themeJasp(p, legend.position = "none")

  return(p)
}

.reglogisticEstimatesInfo <- function(options, addBCA = FALSE) {
  # so we only translate this once
  ciTitle <- if (addBCA)
    gettextf("%1$s%% bca%2$s Confidence interval", 100 * options$coefficientCiLevel, "\u002A")
  else
    gettextf("%s%% Confidence interval", 100 * options$coefficientCiLevel)

  if(options$coefficientCiAsOddsRatio)
    ciTitle <- gettextf("%s <br> (odds ratio scale)", ciTitle)

  seTitle <- gettext("Standard Error")
  if (options$robustSe)
    seTitle <- gettextf("Robust <br> %s", seTitle)

  if (options$method == "enter") {
    paramtitle <- ""
  } else {
    paramtitle <- gettext("Parameter")
  }
  return(list(ciTitle = ciTitle, seTitle = seTitle, paramtitle = paramtitle))
}

# wrapper for wald test
.waldTest <- function(...) {
  result <- try(mdscore::wald.test(...))
  if (jaspBase::isTryError(result)) {
    result <- NA
    return(result)
  }

  return(result[["W"]])
}
