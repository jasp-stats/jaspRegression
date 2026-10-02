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

#' Generalized Linear Model
#'
#' A generalized linear model (GLM) is a flexible extension of ordinary linear regression. A widely used GLM is binary logistic regression. Generally speaking, a GLM consists of a random component and a systematic component:
#'  The random component specifies an appropriate probability distribution for the response variable. For example, a binomial distribution is appropriate for proportions of a total.
#'  The systematic component specifies how the explanatory variables relate to the mean of the response. For example, in binary logistic regression, the logit link function is used to map the responses (i.e. probabilities) to the linear combination of predictors (i.e. linear predictor).
#' ## Family and link
#' The following table summarizes the available distributions, also called families, and link functions, as well as the suitable type of response data. The asterisk * indicates the canonical/default link function for a specific family.
#' 
#' - Bernoulli
#' 	- Links: Logit*, Probit, Cauchit, Complementary log-log, log.
#' 	- Response type: Proportions, counts.
#' - Binomial
#' 	- Links: Logit*, Probit, Cauchit, Complementary Log-Log, Log.
#' 	- Response type: Proportions, counts.
#' - Gaussian
#' 	- Links: Identity*, Log, Inverse.
#' 	- Response type: Continuous.
#' - Gamma
#' 	- Links: Identity, Log, Inverse*.
#' 	- Response type: Positive continuous.
#' - Inverse Gaussian
#' 	- Links: Identity, Log, Inverse, 1/mu^2*.
#' 	- Response type: Positive continuous.
#' - Poisson
#' 	- Identity, Log*, Square-root.
#' 	- Response type: Counts.
#' ## Assumptions
#' - Lack of outliers: All responses were generated from the same process, so that the same model is appropriate for all the observations.
#' - Independence: The responses are independent of each other.
#' - Distribution: The responses come from the specified EDM.
#' - Link function: The correct link function is used.
#' - Linearity: All important explanatory variables are included, and each explanatory variable is included in the linear predictor on the correct scale.
#' - Variance function: The correct variance function is used.
#' - Dispersion parameter: The dispersion parameter is constant.
#' ## Input
#'
#' @param coefficientCi, Provide the confidence intervals around the parameter estimates. The level of the confidence intervals can be specified (default is 95%).
#'    Defaults to \code{FALSE}.
#' @param coefficientEstimate, Selected by default. This gives a table summarizing the model's parameter estimates, standard error around the parameter estimates, the test statistic (t or z), and the p-value.
#'    Defaults to \code{TRUE}.
#' @param covarianceRatio, The degree to which a case influences the variance of the regression parameters. Indicated in the table when the covariance ratio is greater than 3 * k/(n-k).
#'    Defaults to \code{FALSE}.
#' @param covariates, Quantitative variables, such as age, height and IQ.
#' @param dependent, The response variable.
#' @param devianceGoodnessOfFit, Goodness-of-fit test based on deviance residuals, comparing the current model against the saturated model.
#'    Defaults to \code{FALSE}.
#' @param devianceResidualQqPlot, Displays a Q-Q plot of the standardized deviance residuals.
#'    Defaults to \code{FALSE}.
#' @param devianceResidualVsFittedPlot, Displays a scatter plot of the standardized deviance residuals against the fitted values (with constant-information scale transformations).
#'    Defaults to \code{FALSE}.
#' @param devianceResidualVsPredictorPlot, Displays a scatter plot of the standardized deviance residuals against every predictor on its original scale.
#'    Defaults to \code{FALSE}.
#' @param dfbetas, The difference between a parameter estimated using all cases and estimated when one case is excluded. Indicated in the table when the absolute value of DFBETAS is greater than 1.
#'    Defaults to \code{FALSE}.
#' @param dffits, The difference between the predicted value for a case when the model is estimated including or excluding that case. Indicated in the table when the absolute value of DFFITS is greater than 3 * sqrt(k/(n-k)).
#'    Defaults to \code{FALSE}.
#' @param factors, Qualitative variables, such as gender and group memberships.
#' @param family, Distribution of the response variable:
#' @param interceptTerm, Selected by default. This adds an intercept term to the model.
#'    Defaults to \code{TRUE}.
#' @param leverage, The influence of the observed value of the outcome variable over the predicted values. Indicated in the table when the leverage is greater than 3 * k/n.
#'    Defaults to \code{FALSE}.
#' @param mahalanobis, Measures the distance of cases from the mean(s) of the predictor variable(s).
#'    Defaults to \code{FALSE}.
#' @param marginalMeansCi, Width/level of the confidence interval for the estimated marginal means.
#'    Defaults to \code{FALSE}.
#' @param marginalMeansComparison, Value to which the estimated marginal means will be compared to. The default value is set to 0 and can be changed by the user.
#'    Defaults to \code{FALSE}.
#' @param marginalMeansContrast, Create a table for specifying contrasts based on the estimated marginal means. The row indices correspond to the 'Level' column in the estimated marginal means output table. Columns with variable names contain the (combinations of) variable levels for each estimated marginal mean. Columns named Contrast 1, 2, ... are used for specifying the contrasts. To set a contrast between two marginal means, enter -1 and 1 to the corresponding rows. Interactions can be tested by specifying differences between the changes in marginal means of one variable across levels of another variable.
#'    Defaults to \code{FALSE}.
#' @param marginalMeansPAdjustment, Only available when contrasts are specified. To correct for multiple comparison testing and avoid Type I errors, different methods for correcting the p-value are available:
#' @param marginalMeansResponse, Decide whether the estimated marginal means should be computed on the response scale or the untransformed linear scale. The former is selected by default.
#'    Defaults to \code{TRUE}.
#' @param marginalMeansVars, Variables for which the estimated marginal means will be computed.
#' @param partialResidualPlot, Displays partial residual plots across predictors.
#'    Defaults to \code{FALSE}.
#' @param pearsonGoodnessOfFit, Goodness-of-fit test based on Pearson residuals, comparing the current model against the saturated model.
#'    Defaults to \code{FALSE}.
#' @param pearsonResidualQqPlot, Displays a Q-Q plot of the standardized Pearson residuals.
#'    Defaults to \code{FALSE}.
#' @param pearsonResidualVsFittedPlot, Displays a scatter plot of the standardized Pearson residuals against the fitted values (with constant-information scale transformations).
#'    Defaults to \code{FALSE}.
#' @param pearsonResidualVsPredictorPlot, Displays a scatter plot of the standardized Pearson residuals against every predictor on its original scale.
#'    Defaults to \code{FALSE}.
#' @param predictionsSavedToData, Save the predictions of the most complex model as a new column in the data file
#'    Defaults to \code{FALSE}.
#' @param quantileResidualOutlierTable, Top n standardized quantile residuals.
#'    Defaults to \code{FALSE}.
#' @param quantileResidualQqPlot, Displays a Q-Q plot of the standardized quantile residuals.
#'    Defaults to \code{FALSE}.
#' @param quantileResidualVsFittedPlot, Displays a scatter plot of the standardized quantile residuals against the fitted values (with constant-information scale transformations).
#'    Defaults to \code{FALSE}.
#' @param quantileResidualVsPredictorPlot, Displays a scatter plot of the standardized quantile residuals against every predictor on its original scale.
#'    Defaults to \code{FALSE}.
#' @param residualCasewiseDiagnostic, Provides casewise diagnostics for residuals, displaying cases above a threshold based on standardized residuals (default: 3) or Cook's distance (default: 1), or displaying all cases.
#'    Defaults to \code{FALSE}.
#' @param residualStatistic, Displays descriptive statistics of the residuals and predicted values.
#'    Defaults to \code{FALSE}.
#' @param residualsSavedToData, Save the residuals of the most complex model as a new column in the data file.
#'    Defaults to \code{FALSE}.
#' @param residualsSavedToDataType, Choose the type of residual to be appended. Raw residuals are simply the differences between observation and model prediction, standardized residuals divide each residual by an estimate of its standard deviation (using rstandard()), whereas studentized residuals divide each residual by an unbiased estimate of its standard deviation computed by refitting the model without that observation  (using rstudent()).
#' \itemize{
#'   \item \code{"raw"}
#'   \item \code{"standard"}
#'   \item \code{"student"}
#' }
#' @param standardizedResidualOutlierTable, Top n standardized deviance residuals.
#'    Defaults to \code{FALSE}.
#' @param studentizedResidualOutlierTable, Top n studentized deviance residuals.
#'    Defaults to \code{FALSE}.
#' @param tolerance, Indicates how much of a predictor’s variance is not explained by other predictors.
#'    Defaults to \code{FALSE}.
#' @param vif, Variance inflation factor, which indicates how much variance of a predictor is inflated by multicollinearity among the predictor variables.
#'    Defaults to \code{FALSE}.
#' @param weights, Prior weights of the model. Mandatory when the binomial family is selected. In this case, the name changes to total Number of Trials, and the dependent variable now refers to the proportion of success (between 0 and 1).
#' @param workingResponseVsLinearPredictorPlot, Displays a scatter plot of the model's working responses z against the predicted linear predictor values.
#'    Defaults to \code{FALSE}.
GeneralizedLinearModel <- function(
          data = NULL,
          version = "1",
          formula = NULL,
          isNuisance = NULL,
          coefficientCi = FALSE,
          coefficientCiLevel = 0.95,
          coefficientEstimate = TRUE,
          contrasts = list(),
          covarianceRatio = FALSE,
          covariates = list(types = list(), value = list()),
          dependent = list(types = list(), value = ""),
          devianceGoodnessOfFit = FALSE,
          devianceResidualQqPlot = FALSE,
          devianceResidualVsFittedPlot = FALSE,
          devianceResidualVsPredictorPlot = FALSE,
          dfbetas = FALSE,
          dffits = FALSE,
          factors = list(types = list(), value = list()),
          family = "bernoulli",
          interceptTerm = TRUE,
          leverage = FALSE,
          link = "logit",
          mahalanobis = FALSE,
          marginalMeansCi = FALSE,
          marginalMeansCiWidth = 0.95,
          marginalMeansComparison = FALSE,
          marginalMeansComparisonWith = 0,
          marginalMeansContrast = FALSE,
          marginalMeansPAdjustment = "holm",
          marginalMeansResponse = TRUE,
          marginalMeansSd = 1,
          marginalMeansVars = list(types = list(), value = list()),
          modelTerms = list(optionKey = "components", types = list(), value = list()),
          offset = list(types = list(), value = ""),
          otherGlmModel = "multinomialLogistic",
          partialResidualPlot = FALSE,
          pearsonGoodnessOfFit = FALSE,
          pearsonResidualQqPlot = FALSE,
          pearsonResidualVsFittedPlot = FALSE,
          pearsonResidualVsPredictorPlot = FALSE,
          plotHeight = 320,
          plotWidth = 480,
          predictionsSavedToData = FALSE,
          predictionsSavedToDataColumn = "",
          quantileResidualOutlierTable = FALSE,
          quantileResidualOutlierTableTopN = 3,
          quantileResidualQqPlot = FALSE,
          quantileResidualVsFittedPlot = FALSE,
          quantileResidualVsPredictorPlot = FALSE,
          residualCasewiseDiagnostic = FALSE,
          residualCasewiseDiagnosticCooksDistanceThreshold = 1,
          residualCasewiseDiagnosticType = "outliersOutside",
          residualCasewiseDiagnosticZThreshold = 3,
          residualStatistic = FALSE,
          residualsSavedToData = FALSE,
          residualsSavedToDataColumn = "",
          residualsSavedToDataType = "raw",
          seed = 1,
          setSeed = FALSE,
          standardizedResidualOutlierTable = FALSE,
          standardizedResidualOutlierTableTopN = 3,
          studentizedResidualOutlierTable = FALSE,
          studentizedResidualOutlierTableTopN = 3,
          tolerance = FALSE,
          vif = FALSE,
          weights = list(types = list(), value = ""),
          workingResponseVsLinearPredictorPlot = FALSE) {

   defaultArgCalls <- formals(jaspRegression::GeneralizedLinearModel)
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
   optionsWithFormula <- c("isNuisance", "contrasts", "covariates", "dependent", "factors", "family", "marginalMeansPAdjustment", "marginalMeansVars", "modelTerms", "offset", "otherGlmModel", "weights")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspRegression", "GeneralizedLinearModel", "GeneralizedLinearModel.qml", options, version, TRUE))
}