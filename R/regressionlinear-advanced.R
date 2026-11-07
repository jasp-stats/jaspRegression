#
# Copyright (C) 2013-2026 University of Amsterdam
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

# Term specification of the Model section (FactorsForm `advancedAvailable`): terms are defined once, a variable
# with a transformation or coding (also several times: x and log(x)), and every model has its own intercept and
# terms made of them, interactions included. jaspBase::prepareModelTerms() reads it; the models are then
# independent, so nested-model tests are reported only where the models are nested. Only the Enter method is
# supported.
#
# Offsets have their coefficient fixed at 1. An offset of a variable is a formula term (`offset(x)`); a constant offset
# cannot be one, so it is lm()'s `offset` argument and the fit keeps it as attr(fit, "constantOffset"): predict() on new
# data and refits on other rows (the bootstrap) add it themselves. The R^2 of a model with an offset is that of the
# dependent minus the offset.
#
# The learned constants of the terms (mean, sd, knots, ...) are those of the rows as they are: WLS weights are precision
# weights, not frequencies.

.linregIsAdvanced <- function(options) {
  any(vapply(options[["modelTerms"]], function(model) !is.null(model[["definitions"]]), logical(1L)))
}

# A fit of the term specification (.linregGetModelAdvanced() gives it its labels)
.linregIsAdvancedFit <- function(fit) !is.null(attr(fit, "advancedLabels"))

.linregGetModelAdvanced <- function(dependent, options, dataset, weights) {

  prepared <- jaspBase::prepareModelTerms(options[["modelTerms"]], dataset)

  # A model without intercept and with no terms but offsets has nothing to estimate and is skipped. The R^2 change
  # compares a model with the previous fitted one, so nesting is that of the fitted models.
  fitted <- vapply(prepared[["models"]], function(model) !all(model[["terms"]][["offset"]]) || isTRUE(model[["intercept"]]), TRUE)
  if (!any(fitted))
    jaspBase::.quitAnalysis(gettext("No model can be fitted: no model has an intercept or a term with a coefficient (offsets have none)."))
  nested <- if (all(fitted)) prepared[["nested"]] else jaspBase::prepareModelTerms(options[["modelTerms"]][fitted], dataset)[["nested"]]

  model    <- list()
  previous <- NULL # intercept and offsets of the previous fitted model
  for (i in which(fitted)) {
    termsInfo <- prepared[["models"]][[i]][["terms"]]

    # the model's own intercept; the formula's environment is this one, which has `weights` and `offset`
    formula  <- jaspBase::modelTermsFormula(prepared, i, response = dependent)
    constant <- jaspBase::modelTermsConstantOffset(prepared, i)
    offset   <- if (constant != 0) rep(constant, nrow(dataset))
    fit      <- stats::lm(formula, data = dataset, weights = weights, offset = offset, x = TRUE)
    fit$call$offset <- NULL # predict() would evaluate it in another frame

    labels <- jaspBase::modelTermsLabels(prepared, i, names(stats::coef(fit)))
    labels[names(stats::coef(fit)) == "(Intercept)"] <- gettext("(Intercept)") # named as in basic fits

    attr(fit, "constantOffset")      <- constant
    attr(fit, "sampleDependent")     <- isTRUE(prepared[["models"]][[i]][["sampleDependent"]]) # a rank term: no predictions for new data
    attr(fit, "advancedLabels")      <- labels
    attr(fit, "advancedTermLabels")  <- .linregAdvancedTermLabels(fit, termsInfo)
    attr(fit, "advancedOffsets")     <- as.character(termsInfo[["label"]][termsInfo[["offset"]]]) # for the equation
    attr(fit, "advancedCategorical") <- .linregAdvancedCategoricalColumns(fit, names(prepared[["levels"]]))

    # R^2 without an intercept is not centered, and with another offset it is that of another response: no R^2 change
    current    <- list(intercept = attr(stats::terms(fit), "intercept") == 1L, offsets = sort(attr(fit, "advancedOffsets")))
    comparable <- is.null(previous) || identical(previous, current)
    previous   <- current

    model[[length(model) + 1L]] <- list(
      fit        = fit,
      predictors = labels(stats::terms(fit)), # the formula terms (counted, and the keys of VIF and tolerance)
      labels     = as.character(termsInfo[["label"]]),
      number     = i,
      nested     = comparable && (length(model) == 0L || nested[[length(model)]]),
      comparable = comparable
    )
  }

  return(model)
}

# An advanced fit refitted on other rows (the bootstrap): the weights and the constant offset follow the rows (the
# variable offsets are in the formula). lm() looks up `weights` and `offset` in the formula's environment, so the
# formula gets this one.
.linregRefitAdvanced <- function(fit, data, wlsWeights) {

  formula <- stats::formula(fit)
  environment(formula) <- environment()
  weights  <- if (wlsWeights != "") data[[wlsWeights]]
  constant <- attr(fit, "constantOffset")
  offset   <- if (constant != 0) rep(constant, nrow(data))

  return(stats::lm(formula, data = data, weights = weights, offset = offset))
}

# Display label of each formula term (labels(terms(fit))): jaspBase lists the terms in the order lm() uses, the offsets
# (no coefficient, no term label of the fit) last.
.linregAdvancedTermLabels <- function(fit, termsInfo) {
  return(stats::setNames(as.character(termsInfo[["label"]][!termsInfo[["offset"]]]), labels(stats::terms(fit))))
}

# Part and partial correlations of every term of an advanced fit: the fit without the term's design columns
# (a polynomial, spline or factor term has several), so no formula has to be edited; offsets stay in every refit.
.linregGetPartAndPartialCorrelationAdvanced <- function(fit) {

  design       <- fit[["x"]]
  assign       <- attr(design, "assign")
  response     <- stats::model.response(stats::model.frame(fit))
  weights      <- stats::weights(fit)
  offset       <- stats::model.offset(stats::model.frame(fit))
  hasIntercept <- any(assign == 0L)
  R2           <- summary(fit)[["r.squared"]]
  termLabels   <- attr(fit, "advancedTermLabels")

  cors <- list()
  for (term in seq_along(termLabels)) {
    reduced <- design[, assign != term & assign != 0L, drop = FALSE]
    newR2   <- if (ncol(reduced) == 0L) 0 else summary(
      if (hasIntercept) stats::lm(response ~ reduced, weights = weights, offset = offset) else stats::lm(response ~ 0 + reduced, weights = weights, offset = offset)
    )[["r.squared"]]

    sr2  <- R2 - newR2      # squared semi-partial (part) correlation
    pr2  <- sr2 / (1 - newR2) # squared partial correlation
    sign <- if (sum(assign == term) == 1L) sign(stats::coef(fit)[assign == term]) else 1 # terms with several columns have no single sign
    if (is.na(sign)) sign <- 1

    cors[[names(termLabels)[term]]] <- list(name = unname(termLabels[term]), part = sign * sqrt(sr2), partial = sign * sqrt(pr2))
  }

  return(cors)
}

# Whether each design column comes from a term with a categorical variable. jaspBase codes categorical terms as
# numeric contrast matrices, so R's own factor bookkeeping (dataClasses) does not see them.
.linregAdvancedCategoricalColumns <- function(fit, categoricalVariables) {

  termLabels  <- labels(stats::terms(fit))
  categorical <- vapply(termLabels, function(term) any(all.vars(str2lang(term)) %in% categoricalVariables), logical(1L))
  assign      <- attr(fit[["x"]], "assign")

  return(ifelse(assign == 0L, FALSE, categorical[pmax(assign, 1L)]))
}

# .linregGetParametersAndLevels for an advanced fit: the raw parameters of each coefficient are the
# variables of its formula term (so VIF and factor checks find them), the pretty names come from jaspBase.
.linregGetParametersAndLevelsAdvanced <- function(fit) {

  assign    <- attr(fit[["x"]], "assign")
  factors   <- attr(stats::terms(fit), "factors")
  paramsRaw <- lapply(assign, function(term) if (term == 0L) "(Intercept)" else rownames(factors)[factors[, term] > 0L])

  return(list(paramsRaw = paramsRaw, prettyNames = unname(attr(fit, "advancedLabels"))))
}

# Standardized coefficient of design column `column`: the column's own spread (transformed, coded or a product).
.linregGetStandardizedCoefficientAdvanced <- function(fit, column, unstandCoeff) {
  return(unstandCoeff * stats::sd(fit[["x"]][, column]) / stats::sd(stats::model.response(stats::model.frame(fit))))
}

.linregAddAdvancedChangeFootnotes <- function(summaryTable, model) {

  comparable <- vapply(model, function(singleModel) isTRUE(singleModel[["comparable"]]), TRUE)
  nested     <- vapply(model, function(singleModel) isTRUE(singleModel[["nested"]]), TRUE)

  if (!all(comparable))
    summaryTable$addFootnote(gettextf("Some models differ from the model before them in their intercept or offsets: their R%1$s describe another response (or, without an intercept, an uncentered one), so their R%1$s change is not shown.", "\u00B2"))
  if (any(comparable & !nested))
    summaryTable$addFootnote(gettextf("Some models are not nested in the model before them: their R%s change is descriptive and not tested.", "\u00B2"))
}

# The covariance matrix of an advanced fit: its columns are the display labels (models may code a variable
# differently, so raw coefficient names do not line up with the labels across models).
.linregGetCovarianceMatrixAdvanced <- function(fit, columns) {

  namesInModel <- setdiff(names(stats::coef(fit)), "(Intercept)")
  if (length(namesInModel) == 0L)
    return(data.frame())

  labels <- stats::setNames(attr(fit, "advancedLabels"), names(stats::coef(fit)))[namesInModel]
  values <- stats::vcov(fit)[namesInModel, namesInModel, drop = FALSE]
  values[lower.tri(values)] <- NA

  covmatrix <- matrix(NA_real_, length(namesInModel), length(columns), dimnames = list(labels, columns))
  covmatrix[, labels] <- values

  return(cbind(data.frame(name = unname(labels)), covmatrix))
}

.linregAdvancedNotAvailable <- function(container, what) {
  container$setError(gettextf("%s is not available with the term specification.", what))
}
