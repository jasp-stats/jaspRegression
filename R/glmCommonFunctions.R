# as explained in ?is.integer
.is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

# create GLM formula
.createGLMFormula <- function(options, nullModel = FALSE) {

  leftTerm <- options$dependent
  modelTerms <- options$modelTerms
  includeIntercept <- options$interceptTerm

  if (includeIntercept)
    rightTerms <- "1"
  else
    rightTerms <- "-1"

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


.glmComputeModel <- function(jaspResults, dataset, options) {
  if (!is.null(jaspResults[["glmModels"]]))
    return(jaspResults[["glmModels"]]$object)

  # make formulas for the full model and the null model
  ff <- .createGLMFormula(options, nullModel = FALSE)
  nf <- .createGLMFormula(options, nullModel = TRUE)
  # specify family and link
  if (options$family == "bernoulli")
    family <- "binomial"
  else
    family <- options$family
  familyLink <- eval(call(family, link = options$link))
  # compute full and null models
  if (options$weights == "") {
    fullModel <- stats::glm(ff, family = familyLink, data = dataset, weights = NULL)
    nullModel <- stats::glm(nf, family = familyLink, data = dataset, weights = NULL)
  } else {
    fullModel <- stats::glm(ff,
                            family = familyLink,
                            data = dataset,
                            weights = get(options$weights))
    nullModel <- stats::glm(nf,
                            family = familyLink,
                            data = dataset,
                            weights = get(options$weights))
  }

  glmModels <- list("nullModel" = nullModel,
                    "fullModel" = fullModel)
  # combine both models
  jaspResults[["glmModels"]] <- createJaspState()
  jaspResults[["glmModels"]]$dependOn(optionsFromObject = jaspResults[["modelSummary"]])
  jaspResults[["glmModels"]]$object <- glmModels

  return(glmModels)

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
.vif.default <- function(mod, ...) {
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
