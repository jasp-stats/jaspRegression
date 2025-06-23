#
# Copyright (C) 2013-2025 University of Amsterdam
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

# This is a generated file. Don't change it!

#' Bayesian Linear Regression
#'
#' The Bayesian Linear Regression allows you to model a linear relationship between one or more explanatory variable/s and a continuous dependent variable.
#' ## Assumptions
#' - Continuous response variable.
#' - Linearity and additivity: The response variable is linearly related to all predictors and the effects of the predictors are additive.
#' - Independence of errors: The errors are uncorrelated with each other.
#' - Homoscedasticity: The error variance of each predictor is constant across all values of that predictor.
#' - Normality of errors: The errors are normally distributed with mean zero.
#'
#' @param bayesFactorOrder, Compares each model against the model selected here
#' \itemize{
#'   \item \code{"nullModelTop"}: All models are compared to the null model
#'   \item \code{"bestModelTop"}: All models are compared to the best model
#' }
#' @param covariates, Continuous predictor variable(s). If ordinal variables are entered it is assumed that their levels are equidistant. Hence, ordinal variables are treated as continuous predictor variables.
#' @param dependent, Dependent variable
#' @param descriptives, Output table containing the mean, standard deviation, and sample size of the variables selected.
#'    Defaults to \code{FALSE}.
#' @param inclusionProbabilitiesPlot, Shows a histogram of the posterior inclusion probabilities. The dotted line displays the prior inclusion probabilities.
#'    Defaults to \code{FALSE}.
#' @param logPosteriorOddsPlot, Shows a heatmap of the log posterior odds against the model rank.
#'    Defaults to \code{FALSE}.
#' @param marginalPosteriorPlot, Displays a plot of the marginal posterior distribution for each predictor
#'    Defaults to \code{FALSE}.
#' @param modelComplexityPlot,  Shows the relation between model fit and complexity.
#'    Defaults to \code{FALSE}.
#' @param modelPrior, Prior distribution on the models.
#' \itemize{
#'   \item \code{"betaBinomial"}:  Default Beta(a = 1, b = 1).
#'   \item \code{"wilson"}: Default lambda = 1. Equivalent to a Beta binomial with a = 1 and b = lambda * p, where p is the number of predictors in the model.
#'   \item \code{"castillo"}: Default u = 1. Equivalent to a Beta binomial with a = 1 and b = p^u, where p is the number of predictors in the model.
#'   \item \code{"bernoulli"}:  Bernoulli prior. Default p = 0.5.
#'   \item \code{"uniform"}: Uniform prior distribution
#' }
#' @param modelProbabilitiesPlot,  Displays the cumulative distribution function of the model search.
#'    Defaults to \code{FALSE}.
#' @param modelsShown, By default, the output shows all the models computed. There is an option to show the best n models.
#' \itemize{
#'   \item \code{"limited"}
#'   \item \code{"unlimited"}
#' }
#' @param posteriorSummaryPlot, Displays plot of the most likely values of the effect size for each predictor with their corresponding credible interval.
#'    Defaults to \code{FALSE}.
#' @param posteriorSummaryPlotWithoutIntercept, Omits the intercept in the plot display
#'    Defaults to \code{FALSE}.
#' @param posteriorSummaryTable,  Output table containing the Marginal Posterior Summaries of Coefficients. Options available to compare across models or across matched models
#'    Defaults to \code{FALSE}.
#' @param priorRegressionCoefficients, Prior distribution for regression coefficients. Several options are available:
#' \itemize{
#'   \item \code{"ebGlobal"}: Global Empirical Bayes estimates of g in Zellner-Siow g-prior and model probabilities. Uses an EM algorithm to find a common or global estimate of g, averaged over all models. When it is not possible to enumerate all models, the EM algorithm uses only the models sampled under EB-local.
#'   \item \code{"aic"}:  Compare models using the Akaike Information Criterion.
#'   \item \code{"hyperGN"}: A mixture of g-priors that where u = g/n and u Beta(1, alpha/2) to provide consistency when the null model is true.
#'   \item \code{"bic"}: Compare models using the Bayesian Information Criterion.
#'   \item \code{"gPrior"}: Zellner's g-prior.
#'   \item \code{"hyperGLaplace"}: Same as Hyper-g but uses a Laplace approximation to integrate over the prior on g.
#'   \item \code{"hyperG"}: A mixture of g-priors where the prior on g/(1+g) is a Beta(1, alpha/2). This uses the Cephes library for evaluation of the marginal likelihoods and may be numerically unstable for large n or R2 close to 1. Default choice of alpha is 3
#'   \item \code{"jzs"}: Jeffreys-Zellner-Siow prior which uses the Jeffreys prior on sigma and the Zellner-Siow Cauchy prior on the coefficients. The optional parameter can be used to control the squared scale of the prior.
#'   \item \code{"ebLocal"}:  Uses the MLE of g from the marginal likelihood within each model.
#' }
#' @param qqPlot, Displays a Q-Q plot of the model averaged predictions against the residuals
#'    Defaults to \code{FALSE}.
#' @param residualSdsSavedToData, The posterior standard deviation of the residuals.
#'    Defaults to \code{FALSE}.
#' @param residualsSavedToData, The posterior mean of the residuals.
#'    Defaults to \code{FALSE}.
#' @param residualsVsFittedPlot, Plots the residuals of the model averaged predictions against the residuals.
#'    Defaults to \code{FALSE}.
#' @param samplingMethod, Indicates the sampling method to be used. It is recommended to use BAS when the model space can be enumerated.
#' \itemize{
#'   \item \code{"mcmc"}: Samples with replacement via a MCMC algorithm that combines the birth/death random walk with a random swap move to interchange a variable in the model. No. samples indicates the number of MCMC samples to draw. Setting the value to 0 implies the number of MCMC iterations is equal to 10 times the number of models. Sampling stops when min(number of models, MCMC iterations) is reached.
#'   \item \code{"bas"}: Uses Bayesian Adaptive Sampling (without replacement). These can be updated based on estimates of the marginal inclusion. No. models indicates the number of models to sample without replacement. Setting the value to 0 implies the analysis will attempt to enumerate all models.
#' }
#' @param weights, The weights used for weighted least square regression.
RegressionLinearBayesian <- function(
          data = NULL,
          version = "0.95",
          formula = NULL,
          isNuisance = NULL,
          bayesFactorOrder = "bestModelTop",
          bayesFactorType = "BF10",
          bernoulliParam = 0.5,
          betaBinomialParamA = 1,
          betaBinomialParamB = 1,
          castilloParamU = 1,
          covariates = list(types = list(), value = list()),
          dependent = list(types = list(), value = ""),
          descriptives = FALSE,
          effectsType = "allModels",
          gPriorAlpha = 3,
          inclusionProbabilitiesPlot = FALSE,
          jzsRScale = 0.354,
          logPosteriorOddsPlot = FALSE,
          marginalPosteriorPlot = FALSE,
          modelComplexityPlot = FALSE,
          modelPrior = "betaBinomial",
          modelProbabilitiesPlot = FALSE,
          modelTerms = list(optionKey = "components", types = list(), value = list()),
          modelsShown = "limited",
          numModelsShown = 10,
          numberOfModels = 0,
          numericalAccuracy = 1000,
          plotHeight = 320,
          plotWidth = 480,
          posteriorSummaryPlot = FALSE,
          posteriorSummaryPlotCiLevel = 0.95,
          posteriorSummaryPlotWithoutIntercept = FALSE,
          posteriorSummaryTable = FALSE,
          priorRegressionCoefficients = "jzs",
          qqPlot = FALSE,
          residualSdsSavedToData = FALSE,
          residualSdsSavedToDataColumn = "",
          residualsSavedToData = FALSE,
          residualsSavedToDataColumn = "",
          residualsVsFittedPlot = FALSE,
          samples = 0,
          samplingMethod = "bas",
          seed = 1,
          setSeed = FALSE,
          summaryType = "averaged",
          weights = list(types = list(), value = ""),
          wilsonParamLambda = 1) {

   defaultArgCalls <- formals(jaspRegression::RegressionLinearBayesian)
   defaultArgs <- lapply(defaultArgCalls, eval)
   options <- as.list(match.call())[-1L]
   options <- lapply(options, eval)
   defaults <- setdiff(names(defaultArgs), names(options))
   options[defaults] <- defaultArgs[defaults]
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   if (!is.null(formula)) {
      if (!inherits(formula, "formula")) {
         formula <- as.formula(formula)
      }
      options$formula <- jaspBase::jaspFormula(formula, data)
   }
   optionsWithFormula <- c("isNuisance", "covariates", "dependent", "modelTerms", "summaryType", "weights")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspRegression", "RegressionLinearBayesian", "RegressionLinearBayesian.qml", options, version, TRUE))
}