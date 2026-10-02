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

#' Bayesian Logistic Regression
#'
#' The Bayesian Logistic Regression allows you to model a linear relationship between one or more explanatory variable/s and a categorical dependent variable.
#' ## Assumptions
#' - The dependent variable is binary.
#' - The observations are independent of one another.
#' - The logit (log-odds) of the outcome is linearly related to the continuous predictors.
#' - There is little or no multicollinearity among the predictors.
#'
#' @param bayesFactorOrder, Compares each model against the model selected here.
#' \itemize{
#'   \item \code{"bestModelTop"}: All models are compared to the best model.
#'   \item \code{"nullModelTop"}: All models are compared to the null model.
#' }
#' @param covariates, Drag the variables that are covariates to this box. Covariates are continuous variables that have an influence on the dependent variable but are not part of the experimental manipulation.
#' @param dependent, The variable of interest, also called outcome variable. Needs to be categorical for this analysis.
#' @param descriptives, Output table containing the mean, standard deviation, and sample size of the variables selected.
#'    Defaults to \code{FALSE}.
#' @param factors, The variables that are manipulated/define the different groups. These are also called the independent variables.
#' @param inclusionProbabilitiesPlot, Shows a histogram of the posterior inclusion probabilities. The dotted line displays the prior inclusion probabilities.
#'    Defaults to \code{FALSE}.
#' @param logPosteriorOddsPlot, Shows a heatmap of the log posterior odds against the model rank.
#'    Defaults to \code{FALSE}.
#' @param marginalPosteriorPlot, Displays a plot of the marginal posterior distribution for each predictor.
#'    Defaults to \code{FALSE}.
#' @param modelComplexityPlot, Shows the relation between model fit and complexity.
#'    Defaults to \code{FALSE}.
#' @param modelPrior, Prior distribution on the models.
#' \itemize{
#'   \item \code{"uniformSize"}: Uniform prior distribution over the number of included predictors (model size). Each model size receives the same prior probability, which is divided equally among the models of that size. Identical to a Beta binomial with a = 1 and b = 1.
#'   \item \code{"uniform"}: Uniform prior distribution over the models. Each model receives the same prior probability.
#'   \item \code{"betaBinomial"}: Default Beta(a = 1, b = 1).
#'   \item \code{"wilson"}: Default lambda = 1. Equivalent to a Beta binomial with a = 1 and b = lambda * p, where p is the number of predictors in the model.
#'   \item \code{"castillo"}: Default u = 1. Equivalent to a Beta binomial with a = 1 and b = p^u, where p is the number of predictors in the model.
#'   \item \code{"bernoulli"}: Bernoulli prior. Default p = 0.5.
#' }
#' @param modelProbabilitiesPlot, Displays the cumulative distribution function of the model search.
#'    Defaults to \code{FALSE}.
#' @param modelsShown, By default, the output shows all the models computed. There is an option to show the best n models.
#' \itemize{
#'   \item \code{"unlimited"}
#'   \item \code{"limited"}
#' }
#' @param numericalAccuracy, Accuracy of credible intervals. There is an option to select the number of samples used to estimate the credible interval. The more samples the more accurate the credible intervals will be.
#' @param posteriorSummaryPlot, Displays a plot of the most likely values of the effect size for each model with their corresponding credible intervals.
#'    Defaults to \code{FALSE}.
#' @param posteriorSummaryPlotWithoutIntercept, By clicking this box, the intercept is omitted from the plot.
#'    Defaults to \code{FALSE}.
#' @param posteriorSummaryTable, Output table containing the Marginal Posterior Summaries of Coefficients.
#'    Defaults to \code{FALSE}.
#' @param priorRegressionCoefficients, Prior distribution for regression coefficients. Several options are available:
#' \itemize{
#'   \item \code{"aic"}: Compare models using the Akaike Information Criterion.
#'   \item \code{"bic"}: Compare models using the Bayesian Information Criterion.
#'   \item \code{"ebLocal"}: Uses the MLE of g from the marginal likelihood within each model.
#'   \item \code{"gPrior"}: Zellner's g-prior. The default g = n is the unit-information prior.
#'   \item \code{"cch"}
#'   \item \code{"betaPrime"}
#'   \item \code{"instrinsic"}
#'   \item \code{"robust"}
#' }
#' @param qqPlot, Displays a Q-Q plot of the model averaged predictions against the residuals.
#'    Defaults to \code{FALSE}.
#' @param residualSdsSavedToData, Appends the posterior standard deviation of the residuals as a new column in the data file.
#'    Defaults to \code{FALSE}.
#' @param residualsSavedToData, Appends the posterior mean of the residuals as a new column in the data file.
#'    Defaults to \code{FALSE}.
#' @param residualsVsFittedPlot, Plots the residuals of the model averaged predictions against the residuals.
#'    Defaults to \code{FALSE}.
#' @param samplingMethod, Indicates the sampling method to be used. It is recommended to use BAS when the model space can be enumerated.
#' \itemize{
#'   \item \code{"bas"}: Uses Bayesian Adaptive Sampling (without replacement). These can be updated based on estimates of the marginal inclusion. No. models indicates the number of models to sample without replacement. Setting the value to 0 implies the analysis will attempt to enumerate all models.
#'   \item \code{"mcmc"}: Samples with replacement via an MCMC algorithm that combines the birth/death random walk with a random swap move to interchange a variable in the model. No. samples indicates the number of MCMC samples to draw. Setting the value to 0 implies the number of MCMC iterations is equal to 10 times the number of models. Sampling stops when min(number of models, MCMC iterations) is reached.
#' }
#' @param weights, The weights used for weighted least squares regression.
RegressionLogisticBayesian <- function(
          data = NULL,
          version = "1",
          bayesFactorOrder = "bestModelTop",
          bayesFactorType = "BF10",
          bernoulliParam = 0.5,
          betaBinomialParamA = 1,
          betaBinomialParamB = 1,
          castilloParamU = 1,
          cchPriorAlpha = 0.5,
          cchPriorBeta = 2,
          cchPriorS = 0,
          covariates = list(types = list(), value = list()),
          dependent = list(types = list(), value = ""),
          descriptives = FALSE,
          effectsType = "allModels",
          factors = list(types = list(), value = list()),
          gPriorAlpha = 1,
          gPriorType = "n",
          inclusionProbabilitiesPlot = FALSE,
          logPosteriorOddsPlot = FALSE,
          marginalPosteriorPlot = FALSE,
          modelComplexityPlot = FALSE,
          modelPrior = "uniformSize",
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
          priorRegressionCoefficients = "cch",
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

   defaultArgCalls <- formals(jaspRegression::RegressionLogisticBayesian)
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

   optionsWithFormula <- c("covariates", "dependent", "factors", "modelTerms", "summaryType", "weights")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspRegression", "RegressionLogisticBayesian", "RegressionLogisticBayesian.qml", options, version, TRUE))
}