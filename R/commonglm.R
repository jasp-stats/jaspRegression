# Compute Model
.reglogisticComputeModel <- function(jaspResults, dataset, options, ready) {
  if(!is.null(jaspResults[["glmRes"]]) || isFALSE(ready))
    return()

    # Logistic regression
  modelTerms <- options[["modelTerms"]]
  dependent <- options[["dependent"]]
  interceptTerm <- options[["interceptTerm"]]
  if (options[["method"]] == "enter") {
    
    glmRes <- list()
    for (modelIndex in seq_along(modelTerms)) {

      model <- glm(.createLogisticRegFormula(modelTerms[[modelIndex]], dependent, interceptTerm), 
                  family = "binomial", dataset)
      glmRes[[modelIndex]] <- model
      
    }
    
  } else if (options[["method"]] != "enter") {
    nullMod <- glm(.createLogisticRegFormula(modelTerms[[1]], dependent, interceptTerm), 
                   family = "binomial", dataset)
    fullMod <- glm(.createLogisticRegFormula(modelTerms[[length(modelTerms)]], dependent, interceptTerm), 
                   family = "binomial", dataset)
    glmRes  <- .glmStep(nullMod, fullMod, dataset, method = options$method)
  }

  jaspResults[["glmRes"]] <- createJaspState(glmRes)
  jaspResults[["glmRes"]]$dependOn( c("dependent", "method", "modelTerms", "interceptTerm"))
  return(glmRes)
}

# create GLM formula
.createGLMFormula <- function(options, nullModel = FALSE) {
  
  leftTerm <- options$dependent
  modelTerms <- options$modelTerms
  includeIntercept <- options$interceptTerm
  offsetTerm <- options$offset
  
  if (includeIntercept)
    rightTerms <- "1"
  else
    rightTerms <- "-1"
  
  if (offsetTerm != "")
    rightTerms <- c(rightTerms, paste("offset(", offsetTerm, ")", sep = ""))
  
  if (length(modelTerms) == 0) {
    f <- formula(paste(leftTerm, "~", rightTerms))
  } else {
    
    if (nullModel) {
      for (i in seq_along(modelTerms)) {
        nuisance <- modelTerms[[i]][["isNuisance"]]
        if (!is.null(nuisance) && nuisance) {
          term <- modelTerms[[i]][["components"]]
          if (length(term) == 1)
            rightTerms <- c(rightTerms, term)
          else
            rightTerms <- c(rightTerms, paste(term, collapse = ":"))
        }
      }
      
    } else {
      for (i in seq_along(modelTerms)) {
        term <- modelTerms[[i]][["components"]]
        if (length(term) == 1)
          rightTerms <- c(rightTerms, term)
        else
          rightTerms <- c(rightTerms, paste(term, collapse = ":"))
      }
    }
    f <- formula(paste(leftTerm, "~", paste(rightTerms, collapse = "+")))
  }
  return(f)
}

.createLogisticRegFormula <- function(modelOptions, dependent, interceptTerm) {
  # this function outputs a formula name with base64 values as varnames
  f <- NULL

  if (dependent == "")
    f <- 0~1 # mock formula, always works

  modelTerms <- modelOptions$components
  interceptTerm <- interceptTerm
  if (length(modelTerms) == 0) {
    if (interceptTerm)
      f <- formula(paste(dependent, "~ 1"))
    else
      f <- formula(paste(dependent, "~ 0"))
  } else {
    if (interceptTerm)
      t <- character(0)
    else
      t <- "0"
    for (i in seq_along(modelTerms)) {
      term <- modelTerms[[i]]
      if (length(term) == 1)
        t <- c(t, term)
      else
        t <- c(t, paste(unlist(term), collapse = ":"))
    }
    f <- formula(paste(dependent, "~", paste(t, collapse = "+")))
  }
  return(f)
}

.createNullFormula <- function(options) {
  # this function outputs a formula name with base64 values as varnames
  f <- NULL
  dependent <- options$dependent
  if (dependent == "")
    return(NULL)

  modelTerms <- options$modelTerms
  interceptTerm <- options$interceptTerm

  t <- character(0)
  for (i in seq_along(modelTerms)) {
    nui <- modelTerms[[i]][["isNuisance"]]
    if (!is.null(nui) && nui) {
      term <- modelTerms[[i]][["components"]]
      if (length(term) == 1)
        t <- c(t, .v(term))
      else
        t <- c(t, paste(unlist(term), collapse = ":"))
    }
  }
  if (!interceptTerm)
    t <- c(t, "0")
  else
    t <- c(t, "1")

  return(formula(paste(dependent, "~", paste(t, collapse = "+"))))
}

.glmStep <- function(nullModel, fullModel, dataset, method = "enter") {
  # .glmStep function
  # -----------------
  # INPUT: calculated glm objects: nullModel, fullModel, and a dataset
  # nullModel and fullModel should have a data argument
  #
  # OUTPUT: List of glm objects, where the nullModel is the first model
  # (method = enter, forward, stepwise) or the fullModel (method = backward)
  # The last model is the final model that was converged on.

  # first, create temporary environment with dataset for stepAIC() calls
  tempenv <- new.env()
  datname <- as.character(as.list(getCall(fullModel))$data)
  assign(datname, dataset, envir = tempenv)

  null <- nullModel$formula
  full <- fullModel$formula

  assign("nf", null, envir = tempenv)
  assign("ff", full, envir = tempenv)

  if (method == "enter") {
    modlist <- vector("list", 2)
    modlist[[1]] <- nullModel
    modlist[[2]] <- fullModel
  } else if (method == "backward") {
    stepOut <- MASS::stepAIC(fullModel,
                             scope = list(upper=full, lower = null),
                             trace = 0,
                             direction = "backward",
                             keep = function(m, b) list(m))
    modlist <- as.list(stepOut$keep[,1:ncol(stepOut$keep)])
  } else {
    # translate method to stepAIC direction
    direct <- ifelse(method == "forward", method, "both")
    stepOut <- JASPstepAIC(nullModel, full, trace = 0,
                           direction = direct,
                           keep = function(m, b) list(m))
    modlist <- as.list(stepOut$keep[,1:ncol(stepOut$keep)])
  }

  return(modlist)
}

# as explained in ?is.integer
.is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol


.glmComputeModel <- function(jaspResults, dataset, options) {
  if (!is.null(jaspResults[["glmModels"]]))
    return(jaspResults[["glmModels"]]$object)
  
  # make formulas for the full model and the null model
  ff <- .createGLMFormula(options, nullModel = FALSE)
  environment(ff) <- environment()
  nf <- .createGLMFormula(options, nullModel = TRUE)
  environment(nf) <- environment()
  
  weights <- dataset[[options[["weights"]]]]
  
  if (options$family != "other") {
    # specify family and link
    if (options$family == "bernoulli") {
      family <- "binomial"
    }
    else if (options$family == "gamma") {
      family <- "Gamma"
    }
    else if (options$family == "inverseGaussian") {
      family <- "inverse.gaussian"
    }
    else {
      family <- options$family
    }
    
    familyLink <- eval(call(family, link = options$link))
    # compute full and null models
    fullModel <- try(stats::glm(ff,
                                family = familyLink,
                                data = dataset,
                                weights = weights))
    nullModel <- try(stats::glm(nf,
                                family = familyLink,
                                data = dataset,
                                weights = weights))
  } else {
    
    if (options$otherGlmModel == "multinomialLogistic") {
      fullModel <- try(VGAM::vglm(ff,
                                  family = VGAM::multinomial(),
                                  data = dataset,
                                  weights = weights))
      nullModel <- try(VGAM::vglm(nf,
                                  family = VGAM::multinomial(),
                                  data = dataset,
                                  weights = weights))
    }
    
    if (options$otherGlmModel == "ordinalLogistic") {
      fullModel <- try(VGAM::vglm(ff,
                                  family = VGAM::cumulative(link = "logitlink", parallel = TRUE),
                                  data = dataset,
                                  weights = weights))
      nullModel <- try(VGAM::vglm(nf,
                                  family = VGAM::cumulative(link = "logitlink", parallel = TRUE),
                                  data = dataset,
                                  weights = weights))
    }
    
    if (options$otherGlmModel == "firthLogistic") {
      fullModel <- try(logistf::logistf(ff,
                                        data = dataset,
                                        pl = TRUE,
                                        firth = TRUE,
                                        weights = weights))
      nullModel <- try(logistf::logistf(nf,
                                        data = dataset,
                                        pl = TRUE,
                                        firth = TRUE,
                                        weights = weights))
    }
  }
  
  if (jaspBase::isTryError(fullModel)) {
    msg <- .glmFitErrorMessageHelper(fullModel)
    msg <- gettextf("The full model could not be fitted to the data, with the following error message: '%s'", msg)
    jaspBase::.quitAnalysis(msg)
  }
  
  if (jaspBase::isTryError(nullModel)) {
    msg <- .glmFitErrorMessageHelper(nullModel)
    msg <- gettextf("The null model could not be fitted to the data, with the following error message: '%s'", msg)
    jaspBase::.quitAnalysis(msg)
  }
  
  glmModels <- list("nullModel" = nullModel,
                    "fullModel" = fullModel)
  # combine both models
  jaspResults[["glmModels"]] <- createJaspState()
  jaspResults[["glmModels"]]$dependOn(optionsFromObject = jaspResults[["modelSummary"]])
  jaspResults[["glmModels"]]$object <- glmModels
  
  return(glmModels)
}

.glmFitErrorMessageHelper <- function(tryError) {
  msg <- jaspBase::.extractErrorMessage(tryError)
  if(msg == "cannot find valid starting values: please specify some")
    msg <- gettext("Could not find valid starting values. Check feasibility of the model to fit the data.")
  
  return(msg)
}


.hasNuisance <- function(options) {
  return(any(sapply(options$modelTerms, function(x) x[["isNuisance"]])))
}

.glmCreatePlotPlaceholder <- function(container, index, title) {
  jaspPlot            <- createJaspPlot(title = title)
  jaspPlot$status     <- "running"
  container[[index]]  <- jaspPlot
  return()
}


.glmStdResidCompute <- function(model, residType, options) {
  if (residType == "deviance") {
    stdResid <- rstandard(model)
  }
  else if (residType == "Pearson") {
    phiEst <- summary(model)[["dispersion"]]
    resid <- resid(model, type="pearson")
    stdResid <- resid / sqrt(phiEst * (1 - hatvalues(model)))
  }
  else if (residType == "quantile") {
    jaspBase::.setSeedJASP(options)
    resid <- statmod::qresid(model)
    stdResid <- resid / sqrt(1 - hatvalues(model))
  } else {
    stdResid <- NULL
  }
  
  return(stdResid)
}

.glmInsertPlot <- function(jaspPlot, func, ...) {
  p <- try(func(...))
  
  if (inherits(p, "try-error")) {
    errorMessage <- .extractErrorMessage(p)
    jaspPlot$setError(gettextf("Plotting is not possible: %s", errorMessage))
  } else {
    jaspPlot$plotObject <- p
    jaspPlot$status     <- "complete"
  }
}

# Plots: Residuals Q-Q
.glmPlotResQQ <- function(jaspResults, dataset, options, ready, position) {
  
  plotNames <- c("devianceResidualQqPlot", "pearsonResidualQqPlot", "quantileResidualQqPlot")
  if (!ready || !any(unlist(options[plotNames])))
    return()
  
  residNames <- c("deviance", "Pearson", "quantile")
  
  glmPlotResQQContainer <- createJaspContainer(gettext("Q-Q Plots"))
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
                                  title = gettextf("Q-Q plot: Standardized %1s residuals", residNames[[i]]))
        
        .glmInsertPlot(glmPlotResQQContainer[[plotNames[[i]]]],
                       .glmFillPlotResQQ,
                       residType = residNames[[i]],
                       model = glmFullModel,
                       options = options)
      }
    }
  }
  return()
}

.glmFillPlotResQQ <- function(residType, model, options) {
  
  # compute residuals
  stdResid <- .glmStdResidCompute(model = model, residType = residType, options = options)
  
  p <- jaspGraphs::plotQQnorm(stdResid, ablineColor = "darkred", ablineOrigin = TRUE, identicalAxes = TRUE)
  
  return(p)
}


.regressionExportResiduals <- function(container, model, dataset, options, ready) {
  
  if (isFALSE(options[["residualsSavedToData"]]))
    return()
  
  if (is.null(container[["residualsSavedToDataColumn"]]) && options[["residualsSavedToDataColumn"]] != "") {
    
    residuals <- model[["residuals"]] # extract residuals
    
    container[["residualsSavedToDataColumn"]] <- createJaspColumn(columnName = options[["residualsSavedToDataColumn"]])
    container[["residualsSavedToDataColumn"]]$dependOn(options = c("residualsSavedToDataColumn", "residualsSavedToData"))
    container[["residualsSavedToDataColumn"]]$setScale(residuals)
    
  }
  
}

.constInfoTransform <- function(family, x) {
  switch(family,
         "bernoulli" = 1/(sin(sqrt(x))),
         "binomial" = 1/(sin(sqrt(x))),
         "poisson" = sqrt(x),
         "Gamma" = log(x),
         "inverse.gaussian" = 1/sqrt(x),
         "gaussian" = x)
}

.constInfoTransName <- function(family) {
  switch(family,
         "bernoulli" = expression(sin^-1 * sqrt("Fitted values")),
         "binomial" = expression(sin^-1 * sqrt("Fitted values")),
         "poisson" = expression(sqrt("Fitted values")),
         "Gamma" = expression(log("Fitted values")),
         "inverse.gaussian" = expression(1/sqrt("Fitted Values")),
         "gaussian" = "Fitted values")
}

.capitalize <- function(x) {
  x_new <- paste(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)), sep="")
  return(x_new)
}

# function for multicollineary statistics, taken from the source code of car::vif
# car version: 3.0-13 see https://rdrr.io/cran/car/src/R/vif.R
# the reason is to remove the dependency on the car package. When importing the entire car package,
# JASP crashes when loading the regression module (at least on Windows 10).

.vif.default <- function(mod, ...) {
  # modified to fix bug: https://github.com/jasp-stats/jasp-test-release/issues/2487
  if (length(coef(mod)) == 0) {
    stop(gettext("There are no predictors to test for multicollinearity"))
  }
  # end modification
  if (any(is.na(coef(mod))))
    stop ("there are aliased coefficients in the model")
  v <- vcov(mod)
  assign <- attr(model.matrix(mod), "assign")
  if (names(coefficients(mod)[1]) == "(Intercept)") {
    v <- v[-1, -1]
    assign <- assign[-1]
  }
  else warning("No intercept: vifs may not be sensible.")
  terms <- labels(terms(mod))
  n.terms <- length(terms)
  if (n.terms < 2) stop("model contains fewer than 2 terms")
  R <- cov2cor(v)
  detR <- det(R)
  result <- matrix(0, n.terms, 3)
  rownames(result) <- terms
  colnames(result) <- c("GVIF", "Df", "GVIF^(1/(2*Df))")
  for (term in 1:n.terms) {
    subs <- which(assign == term)
    result[term, 1] <- det(as.matrix(R[subs, subs])) *
      det(as.matrix(R[-subs, -subs])) / detR
    result[term, 2] <- length(subs)
  }
  if (all(result[, 2] == 1)) result <- result[, 1]
  else result[, 3] <- result[, 1]^(1/(2 * result[, 2]))
  result
}

# message for estimated marginal means table
.emmMessageTestNull     <- function(value)  gettextf("P-values correspond to test of null hypothesis against %s.", value)
.emmMessageAveragedOver <- function(terms)  gettextf("Results are averaged over the levels of: %s.",paste(terms, collapse = ", "))
.messagePvalAdjustment  <- function(adjustment) {
  if (adjustment == "none") {
    return(gettext("P-values are not adjusted."))
  }
  adjustment <- switch(adjustment,
                       "holm"       = gettext("Holm"),
                       "hommel"     = gettext("Homel"),
                       "hochberg"   = gettext("Hochberg"),
                       "mvt"        = gettext("Multivariate-t"),
                       "tukey"      = gettext("Tukey"),
                       "BH"         = gettext("Benjamini-Hochberg"),
                       "BY"         = gettext("Benjamini-Yekutieli"),
                       "scheffe"    = gettext("Scheffé"),
                       "sidak"      = gettext("Sidak"),
                       "dunnettx"   = gettext("Dunnett"),
                       "bonferroni" = gettext("Bonferroni")
  )
  return(gettextf("P-values are adjusted using %s adjustment.", adjustment))
}



.confusionMatAddColInfo <- function(table, levs, type) {
  table$addColumnInfo(name = "pred0", title = paste0(levs[1]), type = type, overtitle = gettext("Predicted"))
  table$addColumnInfo(name = "pred1", title = paste0(levs[2]), type = type, overtitle = gettext("Predicted"))
}

# Helper functions for the above.
.lrtest <- function(glmModel1, glmModel2) {
  # likelihood ratio test for model against null model
  if (glmModel1[["df.residual"]] > glmModel2[["df.residual"]]) {
    superModel <- glmModel2
    subModel <- glmModel1
  } else {
    superModel <- glmModel1
    subModel <- glmModel2
  }

  chisq <- max(0, subModel[["deviance"]] - superModel[["deviance"]])
  df <- subModel[["df.residual"]] - superModel[["df.residual"]]

  if (chisq == 0 || df == 0)
    p <- NULL
  else
    p <- 1-pchisq(chisq, df)
  return(list(stat = chisq, df = df, pval = p))
}

.hasNuisance <- function(options) {
  return(any(sapply(options$modelTerms, function(x) x[["isNuisance"]])))
}

.mcFadden <- function(glmModel, nullModel) {
  # https://eml.berkeley.edu/reprints/mcfadden/zarembka.pdf
  if (.isInterceptOnly(glmModel)) {
    # intercept-only model needs fix because of computer precision limits
    return(0)
  } else
    return(max(c(0,1-(glmModel[["deviance"]]/nullModel[["deviance"]]))))
}

.nagelkerke <- function(glmModel, nullModel) {
  # https://doi.org/10.1093/biomet/78.3.691
  if (.isInterceptOnly(glmModel)) {
    # intercept-only model needs fix because of computer precision limits
    return(NULL)
  } else {
    l0 <- -0.5*nullModel[["deviance"]]
    lm <- as.numeric(logLik(glmModel))
    n  <- length(glmModel[["y"]])
    coxSnell <- .coxSnellCompute(l0, lm, n)
    denom <- -expm1(2*l0/n)
    return(max(c(0,coxSnell / denom)))
  }
}

.tjur <- function(glmModel) {
  # http://dx.doi.org/10.1198/tast.2009.08210
  ps <- predict(glmModel, type = "response")
  ys <- glmModel[["y"]]
  return(abs(mean(ps[as.logical(ys)])-mean(ps[!as.logical(ys)])))
}

.coxSnell <- function(glmModel, nullModel) {
  if (.isInterceptOnly(glmModel)) {
    # intercept-only model needs fix because of computer precision limits
    return(NULL)
  } else {
    l0 <- -0.5*nullModel[["deviance"]]
    lm <- as.numeric(logLik(glmModel))
    n  <- length(glmModel[["y"]])
    coxSnell <- .coxSnellCompute(l0, lm, n)
    return(max(c(0,coxSnell)))
  }
}

.coxSnellCompute <- function(l0, lm, n) {
  return(-expm1(2*(l0 - lm)/n))
}

.bic <- function(glmModel) {
  return(log(length(glmModel[["y"]]))*length(coef(glmModel))+glmModel[["deviance"]])
}

.stdEst <- function(glmModel, type = "X") {
  # This function fits a new model with scaled variables and outputs the coeffs:
  # type = "X" : covariates scaled
  # type = "Y" : outcome scaled
  # type = "XY" : covariates and outcomes scaled
  # NB: factors (dummy-coded) will never be scaled.
  if (attr(glmModel[["terms"]], "intercept"))
    b <- summary(glmModel)[["coefficients"]][-1,1]
  else
    b <- summary(glmModel)[["coefficients"]][,1]

  factors <- names(glmModel[["xlevels"]])
  xmod <- glmModel[["model"]][!names(glmModel[["model"]]) %in% factors][,-1]
  xfac <- glmModel[["model"]][names(glmModel[["model"]]) %in% factors]
  ymod <- glmModel[["model"]][1]

  if (type == "X")
    stdDat <- cbind(ymod, scale(xmod), xfac)
  else if (type == "Y")
    stdDat <- cbind(scale(ymod), xmod, xfac)
  else if (type == "XY")
    stdDat <- cbind(scale(ymod), scale(xmod), xfac)

  names(stdDat) <- names(glmModel[["model"]])

  stdMod <- stats::glm(formula = glmModel[["formula"]], data = stdDat,
                       family = glmModel[["family"]])

  return(coef(stdMod))
}

.confusionMatrix <- function(glmModel, cutoff=0.5) {
  cMat <- list()
  pred <- predict(glmModel,type = "response")
  obs  <- glmModel$y
  h    <- hmeasure::HMeasure(obs, pred, threshold = cutoff)
  m    <- matrix(c(h[["metrics"]][["TN"]], h[["metrics"]][["FN"]],
                   h[["metrics"]][["FP"]], h[["metrics"]][["TP"]]), 2)
  dimnames(m) <- list("Observed" = c(0, 1), "Predicted" = c(0, 1))
  h[["metrics"]][["Brier"]] <- .brierScore(obs, pred)
  cMat[["matrix"]]  <- m
  cMat[["metrics"]] <- h[["metrics"]]
  return(cMat)
}

.brierScore <- function(obs, pred) {
  return(sum((pred - obs)^2) / length(pred))
}

.formatTerm <- function(term, glmModel) {
  # input: string of model term & glmObj
  vars <- names(glmModel[["model"]][-1])

  if (attr(glmModel[["terms"]], "intercept"))
    vars <- c(vars, gettext("(Intercept)"))

  # escape special regex characters
  vars <- gsub("(\\W)", "\\\\\\1", vars, perl=TRUE)

  # regex patterns
  pat1 <- paste0("\\:","(?=(",paste(vars, collapse = ")|("),"))")
  pat2 <- paste0("(?<=(",paste(vars, collapse = ")|("),"))")

  # split up string into components
  spl  <- strsplit(term, pat1, perl = TRUE)[[1]]
  spl2 <- lapply(spl, function(t) strsplit(t, pat2, perl = TRUE))

  # format and add back together
  col <- lapply(spl2, function(s) {
    if (length(unlist(s)) > 1) {
      varname <- .unv(unlist(s)[1])
      levname <- unlist(s)[2]
      return(paste0(varname, " (", levname, ")"))
    } else
      return(.unv(unlist(s)))
  })
  col2 <- paste(unlist(col), collapse = " * ")

  return(col2)
}

.predLevel <- function(glmModel) {
  predVar <- as.character(glmModel[["terms"]])[2]
  return(levels(glmModel[["data"]][[predVar]])[2])
}

.glmLogRegRibbon <- function(logRes, var, ciInt = 0.95) {
  # This function calculates the estimation & CI datapoints for logreg plot
  fac     <- FALSE
  factors <- names(logRes$xlevels)
  mf      <- model.frame(logRes)
  outcome <- logRes$terms[[2]]
  cDat    <- mf[,!(colnames(mf) %in% c(factors, outcome))]
  if (length(cDat) == nrow(mf)) {
    cm        <- mean(cDat)
    names(cm) <- colnames(mf)[!(colnames(mf) %in% c(factors, outcome))]
  } else
    cm <- colMeans(cDat)


  if (length(factors) == 0 || !(var %in% factors)) {
    # Variable of interest is continuous
    # create 101-length data frame of repeated colmeans
    newDat <- data.frame(matrix(rep(cm, 101), nrow=101, byrow=TRUE))
    colnames(newDat) <- names(cm)
    # add factors to this data
    if (length(factors) > 0) {
      for (f in factors) {
        col <- factor(rep(logRes[["xlevels"]][[f]][1], 101),
                      levels = logRes[["xlevels"]][[f]])
        newDat <- data.frame(newDat, col)
        colnames(newDat)[ncol(newDat)] <- f
      }
    }
    # then change the variable of interest to a nice range
    vd <- pretty(mf[[var]])
    xs <- seq(min(vd), max(vd), length.out = 101)
    newDat[[var]] <- xs
  } else {
    # variable is factor
    fac <- TRUE
    levs <- paste0(.unv(var), logRes[["xlevels"]][[var]])
    nlevs <- length(levs)

    # create a new data frame of nlevs length
    newDat <- data.frame(matrix(rep(cm, nlevs), nrow=nlevs, byrow=TRUE))
    colnames(newDat) <- names(cm)
    # add factors to this data
    if (length(factors) > 0) {
      for (f in factors) {
        col <- factor(rep(logRes[["xlevels"]][[f]][1], nlevs),
                      levels = logRes[["xlevels"]][[f]])
        newDat <- data.frame(newDat, col)
        colnames(newDat)[ncol(newDat)] <- f
      }
    }
    # then change the factor of interest to a nice range
    xs <- factor(logRes[["xlevels"]][[var]],
                 levels = logRes[["xlevels"]][[var]])
    newDat[[var]] <- xs
  }

  pred <- predict(logRes, newdata = newDat, type = "link", se.fit = TRUE)
  ys   <- .invLogit(pred$fit)
  critValue <- qnorm(1 - (1 - ciInt) / 2)
  lo <- .invLogit(pred$fit - critValue * pred$se.fit)
  hi <- .invLogit(pred$fit + critValue * pred$se.fit)
  outFrame <- data.frame(x = xs, y = ys, lo = lo, hi = hi)

  attr(outFrame, "factor") <- fac

  return(outFrame)
}

.invLogit <- function(x) {
  return(1/(1 + exp(-x)))
}

.glmRobustSE <- function(glmModel) {
  # Robust SE courtesy of Dan Gillen (UC Irvine)
  if (is.matrix(glmModel[["x"]]))
    xmat <- glmModel[["x"]]
  else {
    mf   <- model.frame(glmModel)
    xmat <- model.matrix(terms(glmModel), mf)
  }

  umat      <- residuals(glmModel, "working") * glmModel[["weights"]] * xmat
  modelv    <- summary(glmModel)[["cov.unscaled"]]
  robustCov <- modelv%*%(t(umat)%*%umat)%*%modelv
  dimnames(robustCov) <- dimnames(vcov(glmModel))

  ##	Format the model output with p-values and CIs
  s <- summary(glmModel)
  robustSE <- sqrt(diag(robustCov))
  return(robustSE)
}


# Table: Influential cases
.glmInfluenceTable <- function(jaspResults, model, dataset, options, ready, position, logisticRegression = FALSE) {

  tableOptionsOn <- c(options[["dfbetas"]],
                      options[["dffits"]],
                      options[["covarianceRatio"]],
                      options[["leverage"]],
                      options[["mahalanobis"]])
  
  nModels <- length(options$modelTerms)
  if (!ready || !options[["residualCasewiseDiagnostic"]] || 
      length(unlist(options$modelTerms[[nModels]][["components"]])) == 0)
    return()
  
  
  tableOptions <- c("dfbetas", "dffits", "covarianceRatio", "leverage", "mahalanobis")
  tableOptionsClicked <- tableOptions[tableOptionsOn]
  tableOptionsClicked <- c("cooksDistance", tableOptionsClicked)
  
  if (is.null(jaspResults[["influenceTable"]])) {
    influenceTable <- createJaspTable(gettext("Influential Cases"))
    influenceTable$dependOn(c(tableOptions, "dependent", "method", "modelTerms", "interceptTerm", 
                              "residualCasewiseDiagnostic", "residualCasewiseDiagnosticType",
                              "residualCasewiseDiagnosticZThreshold", 
                              "residualCasewiseDiagnosticCooksDistanceThreshold"))
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
    
    
    depType <- if (isFALSE(logisticRegression))  "number" else "string"
    influenceTable$addColumnInfo(name = "caseN", title = "Case Number", type = "integer")
    influenceTable$addColumnInfo(name = "stdResidual", title = gettext("Std. Residual"),   type = "number", format = "dp:3")
    influenceTable$addColumnInfo(name = "dependent",   title = options$dependent,          type = depType)
    influenceTable$addColumnInfo(name = "predicted",   title = gettext("Predicted Value"), type = "number")
    influenceTable$addColumnInfo(name = "residual", title = gettext("Residual"),   type = "number", format = "dp:3")
    
    colNameList  <- c()
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
  
  tempResult <- influenceRes[["infmat"]][, colInd]
  resultContainsNaN <- any(is.nan(tempResult))
  tempResult[which(is.nan(tempResult))] <- NA
  influenceResData <- as.data.frame(tempResult)
  colnames(influenceResData)[1:length(colInd)] <- colNames[1:length(colInd)]
  
  influenceResData[["caseN"]] <- seq.int(nrow(influenceResData))
  influenceResData[["stdResidual"]] <- rstandard(model)
  influenceResData[["dependent"]] <- model.frame(model)[[options$dependent]]
  influenceResData[["predicted"]] <- model$fitted.values
  influenceResData[["residual"]] <- model$residual

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
    if (sum(influenceResSig, na.rm = TRUE) > 0) {
      for (thisCol in colnames(influenceResSig)) {
        if (sum(influenceResSig[, thisCol], na.rm = TRUE) > 0) 
          influenceTable$addFootnote(
            gettext("Potentially influential case, according to the selected influence measure."), 
            colNames = thisCol,
            rowNames = rownames(influenceResData)[influenceResSig[, thisCol]],
            symbol = "*"
          )
      }
    }
    
    if (resultContainsNaN) {
      influenceTable$addFootnote(
        gettext("Influence measures could not be computed for some cases due to extreme values, try another measure.")
      )
    }
  }
}

.reglogisticVovkSellke <- function(table, options) {
  table$addColumnInfo(name = "vsmpr",   title = gettextf("VS-MPR%s", "\u002A"), type = "number")
  message <- gettextf("Vovk-Sellke Maximum <em>p</em>-Ratio: Based on the <em>p</em>-value,
  the maximum possible odds in favor of H%1$s over H%2$s equals
  1/(-e <em>p</em> log(<em>p</em>)) for <em>p</em> %3$s .37
  (Sellke, Bayarri, & Berger, 2001).", "\u2081", "\u2080", "\u2264")
  table$addFootnote(message, symbol = "\u002A")
}

.reglogisticSetError <- function(res, table) {
  if(isTryError(res))
    table$setError(.extractErrorMessage(res))
}


## functions in package car 3.0-12
# Durbin-Watson
.durbinWatsonTest.lm <- function(model, max.lag=1, simulate=TRUE, reps=1000,
                                method=c("resample","normal"),
                                alternative=c("two.sided", "positive", "negative"), ...){
  method <- match.arg(method)
  alternative <- if (max.lag == 1) match.arg(alternative)
  else "two.sided"
  residuals <- residuals(model)
  if (any(is.na(residuals))) stop ('residuals include missing values')
  n <- length(residuals)
  r <- dw <-rep(0, max.lag)
  den <- sum(residuals^2)
  for (lag in 1:max.lag){
    dw[lag] <- (sum((residuals[(lag+1):n] - residuals[1:(n-lag)])^2))/den
    r[lag] <- (sum(residuals[(lag+1):n]*residuals[1:(n-lag)]))/den
  }
  if (!simulate){
    result <- list(r=r, dw=dw)
    class(result) <- "durbinWatsonTest"
    result
  }
  else {
    S <- summary(model)$sigma
    X <- unname(model.matrix(model)) # CHANGE: unname
    mu <- fitted.values(model)

    # START OF CHANGES
    # the original code is horrible for larger datasets.
    # It creates all bootstrap datasets at once (e.g., matrix(sample(residuals, n*reps, replace=TRUE), n, reps))
    # we do this one at a time.

    # since the design matrix is constant across bootstraps, we precompute solve(t(X) %*% X) %*% t(X) with
    # a QR factorization. Otherwise lm does this again and again in every step.
    qrX <- qr(X)
    R <- qr.R(qrX)
    Rinv <-  backsolve(r = R, x = diag(ncol(R))) # efficiently invert triangular matrix (see https://stackoverflow.com/a/31772085 )
    # this is the same as solve(t(X) %*% X) %*% t(X) but then computed more efficiently
    inv_XTX_XT <- tcrossprod(tcrossprod(Rinv), X)
    # all.equal(inv_XTX_XT, solve(t(X) %*% X) %*% t(X), check.attributes = FALSE)  # assertion 1
    # y <- model$model[[1]]
    # resids <- y - X %*% (inv_XTX_XT %*% y)
    # all.equal(unname(residuals), c(resids)) # assertion 2

    # start a progressbar if we expect this to take a while
    if (length(residuals) > 50000L) {
      startProgressbar(expectedTicks = reps, label = gettext("Computing Durbin-Watson Statistics"))
      tick <- progressbarTick
    } else {
      tick <- function() {}
    }

    # drop names to avoid copying them
    bootResult <- boot::boot(unname(residuals), statistic = function(data, indices, inv_XTX_XT, max.lag, tick) {
      y <- data[indices]
      resids <- c(y - X %*% (inv_XTX_XT %*% y)) # NOTE: parenthesis are super important here because doing X %*% inv_XTX_XT first allocates an n by n matrix
      # original code
      # E <- c(residuals(lm(y ~ X - 1)))
      # all.equal(unname(E), resids)
      .durbinWatsonTest.default(resids, max.lag = max.lag)
      tick()
    }, R = reps, inv_XTX_XT = inv_XTX_XT, max.lag = max.lag, tick = tick)
    DW <- bootResult$t

    # ORIGINAL CODE
    # Y <- if (method == "resample")
    #   matrix(sample(residuals, n*reps, replace=TRUE), n, reps) + matrix(mu, n, reps)
    # else matrix(rnorm(n*reps, 0, S), n, reps) + matrix(mu, n, reps)
    # E <- residuals(lm(Y ~ X - 1))
    # DW <- apply(E, 2, .durbinWatsonTest.default, max.lag=max.lag)
    # if (max.lag == 1) DW <- rbind(DW)
    # END OF CHANGES
    p <- rep(0, max.lag)
    if (alternative == 'two.sided'){
      for (lag in 1:max.lag) {
        p[lag] <- (sum(dw[lag] < DW[lag,]))/reps
        p[lag] <- 2*(min(p[lag], 1 - p[lag]))
      }
    }
    else if (alternative == 'positive'){
      for (lag in 1:max.lag) {
        p[lag] <- (sum(dw[lag] > DW[lag,]))/reps
      }
    }
    else {
      for (lag in 1:max.lag) {
        p[lag] <- (sum(dw[lag] < DW[lag,]))/reps
      }
    }
    result <- list(r=r, dw=dw, p=p, alternative=alternative)
    class(result)<-"durbinWatsonTest"
    result
  }
}

.durbinWatsonTest.default <- function(model, max.lag=1, ...){
  # in this case, "model" is the residual vectors
  if ((!is.vector(model)) || (!is.numeric(model)) ) stop("requires vector of residuals")
  if (any(is.na(model))) stop ('residuals include missing values')
  n <-  length(model)
  dw <- rep(0, max.lag)
  den <- sum(model^2)
  for (lag in 1:max.lag){
    dw[lag] <- (sum((model[(lag+1):n] - model[1:(n-lag)])^2))/den
  }
  dw
}

# test if a model is intercept only
.isInterceptOnly <- function(model) {
  terms <- try(terms(model))
  if (jaspBase::isTryError(terms)) {
    terms <- try(terms(model[["formula"]]))

    if (jaspBase::isTryError(terms)) {
      stop("Something went wrong; Cannot tell if a model is intercept-only.")
    }
  }

  intercept <- attr(terms, "intercept")
  labels <- attr(terms, "term.labels")

  # test if we have intercept and no other predictors
  return(intercept && length(labels) == 0)
}
