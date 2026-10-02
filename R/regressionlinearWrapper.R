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

#' Linear Regression
#'
#' Linear regression allows the user to model a linear relationship between one or more explanatory variable(s) (predictors) and a continuous dependent (response) variable.
#' ## Assumptions
#' - Continuous response variable.
#' - Linearity and additivity: The response variable is linearly related to all predictors and the effects of the predictors are additive.
#' - Independence of residuals: The residuals are uncorrelated with each other.
#' - Homoscedasticity: The error variance of each predictor is constant across all values of that predictor.
#' - Normality of residuals: The residuals are normally distributed with mean zero.
#'
#' @param coefficientBootstrap, Apply bootstrapped estimation. By default, the number of replications is set to 1000, but this can be changed to the desired number.
#'    Defaults to \code{FALSE}.
#' @param coefficientCi, Includes confidence intervals for the estimated mean difference. By default the confidence level is set to 95%, but this can be changed to the desired percentage.
#'    Defaults to \code{FALSE}.
#' @param coefficientEstimate, Unstandardized and standardized coefficient estimates, standard errors, t-values, and their corresponding p-values.
#'    Defaults to \code{TRUE}.
#' @param collinearityDiagnostic, Collinearity statistics, eigenvalues, condition indices, and variance proportions.
#'    Defaults to \code{FALSE}.
#' @param collinearityStatistic, Displays Tolerance and Variance Inflation Factor for each predictor in the model in order to assess multicollinearity.
#'    Defaults to \code{FALSE}.
#' @param covarianceMatrix, Displays the covariance matrix of the predictor variables, per model.
#'    Defaults to \code{FALSE}.
#' @param covarianceRatio, The degree to which a case influences the variance of the regression parameters. Indicated in the table when the covariance ratio is greater than 3 * k/(n-k).
#'    Defaults to \code{FALSE}.
#' @param covariates, Continuous predictor variable(s). If ordinal variables are entered it is assumed that their levels are equidistant. Hence, ordinal variables are treated as continuous predictor variables.
#' @param dependent, Dependent variable.
#' @param descriptivePlotErrorBar, Show error bars on the plot. For scatter plots the shaded band represents the confidence/prediction interval of the regression line.
#'    Defaults to \code{TRUE}.
#' @param descriptivePlotHorizontalAxis, Predictor to display on the horizontal axis. Scale predictors will be split into groups according to the Scale Predictor Grouping settings.
#' @param descriptivePlotScaleGroupingMethod, Standard deviation: groups are defined by equal-width intervals around the mean (e.g., Low = below mean−SD, Medium = mean±SD, High = above mean+SD). Percentile: groups are equal-frequency bins based on quantile cut-points.
#' @param descriptivePlotScaleGroups, Number of groups to create for each scale predictor. Minimum 2, maximum 10.
#' @param descriptivePlotSeparateLines, Predictor whose levels are shown as separate lines. Scale predictors will be binned into groups.
#' @param descriptivePlotSeparatePlot, Predictor whose levels are shown as separate plots. Scale predictors will be binned into groups.
#' @param descriptives, Samples size, sample mean, sample standard deviation, and standard error of the mean.
#'    Defaults to \code{FALSE}.
#' @param dfbetas, The difference between a parameter estimated using all cases and estimated when one case is excluded. Indicated in the table when the absolute value of DFBETAS is greater than 1.
#'    Defaults to \code{FALSE}.
#' @param dffits, The difference between the predicted value for a case when the model is estimated including or excluding that case. Indicated in the table when the absolute value of DFFITS is greater than 3 * sqrt(k/(n-k)).
#'    Defaults to \code{FALSE}.
#' @param equationTable, Displays regression equations for each model.
#'    Defaults to \code{FALSE}.
#' @param fChange, Change in F-statistic between the different models, with corresponding significance test.
#'    Defaults to \code{FALSE}.
#' @param factors, Categorical predictor variable(s). Ordinal variables here are treated as categorical predictor variables, thus, the ordinal information is ignored.
#' @param interceptTerm, Include the intercept in the regression model.
#'    Defaults to \code{TRUE}.
#' @param leverage, The influence of the observed value of the outcome variable over the predicted values. Indicated in the table when the leverage is greater than 3 * k/n.
#'    Defaults to \code{FALSE}.
#' @param mahalanobis, Measures the distance of cases from the mean(s) of the predictor variable(s).
#'    Defaults to \code{FALSE}.
#' @param marginalPlot, Visualization of how a change in a predictor affects the predicted outcome, holding other variables constant.
#'    Defaults to \code{FALSE}.
#' @param marginalPlotCi, Displays the specified confidence interval around the regression line on the plot.
#'    Defaults to \code{FALSE}.
#' @param method, Specify the method for entering predictors into the model. For the Enter method, predictors are added as specified in the Model tab. With Forward, Backward, or Stepwise methods, predictors listed in Model 1 are treated as candidate variables that can be added to or removed from the model based on statistical criteria (found under Method Specification). Predictors listed in Model 0 are forced into the model and remain in all steps, regardless of their fit.
#' \itemize{
#'   \item \code{"enter"} (default) : All predictors are entered into the models as specified in the Model tab.
#'   \item \code{"backward"}: All predictors are entered simultaneously, and then removed sequentially based on the criterion specified in Stepping method criteria.
#'   \item \code{"forward"}: Predictors are entered sequentially based on the criterion specified in Stepping method criteria.
#'   \item \code{"stepwise"}: Predictors are entered sequentially based on the criterion specified in Stepping method criteria; after each step, the least useful predictor is removed.
#' }
#' @param modelAICBIC, Displays Akaike Information Criterion and Bayesian Information Criterion.
#'    Defaults to \code{FALSE}.
#' @param modelFit, Separate ANOVA table for each model, i.e., each step in Backward, Forward, and Stepwise regression.
#'    Defaults to \code{TRUE}.
#' @param modelTerms, Model terms: The independent variables in the model. By default, all the main effects of the specified independent variables are included in the model. To include interactions, click multiple variables by holding the ctrl/command button while clicking, and drag those into the Model Terms box.
#' @param partAndPartialCorrelation, Semipartial and partial correlations.
#'    Defaults to \code{FALSE}.
#' @param partialResidualPlot, These plots are scatterplots of the residuals from two regressions - regressing the dependent variable on all other predictors, and regressing a particular predictor (i.e., as dependent variable) on all other predictors-then plotting the residuals against each other.
#'    Defaults to \code{FALSE}.
#' @param partialResidualPlotCi, Displays the specified confidence interval around the regression line on the plot.
#'    Defaults to \code{FALSE}.
#' @param predictionsSavedToData, Save the predictions of the most complex model as a new column in the data file
#'    Defaults to \code{FALSE}.
#' @param quadraticTerms, Include quadratic terms for the covariates in each model.
#'    Defaults to \code{FALSE}.
#' @param rSquaredChange, Change in R squared between the different models, with corresponding significance test.
#'    Defaults to \code{TRUE}.
#' @param residualCasewiseDiagnostic, Provides casewise diagnostics for residuals, displaying cases above a threshold based on standardized residuals (default: 3) or Cook's distance (default: 1), or displaying all cases.
#'    Defaults to \code{FALSE}.
#' @param residualDurbinWatson, Durbin-Watson statistic to test the autocorrelation of the residuals.
#'    Defaults to \code{FALSE}.
#' @param residualHistogramPlot, Displays a histogram of residual values.
#'    Defaults to \code{FALSE}.
#' @param residualHistogramStandardizedPlot, Use standardized residuals instead.
#'    Defaults to \code{TRUE}.
#' @param residualQqPlot, Displays Q-Q plot of the standardized residuals. The confidence band shows the expected range of residuals under normality; points outside the band suggest deviations from normality.
#'    Defaults to \code{FALSE}.
#' @param residualStatistic, Displays descriptive statistics of the residuals and predicted values.
#'    Defaults to \code{FALSE}.
#' @param residualVsCovariatePlot, Scatterplot of the values of the residuals against the predictor variables.
#'    Defaults to \code{FALSE}.
#' @param residualVsDependentPlot, Scatterplot of the values of the residuals against the dependent variable.
#'    Defaults to \code{FALSE}.
#' @param residualVsFittedPlot, Scatterplot of the values of the residuals against the predicted values.
#'    Defaults to \code{FALSE}.
#' @param residualsSavedToData, Save the residuals of the most complex model as a new column in the data file.
#'    Defaults to \code{FALSE}.
#' @param residualsSavedToDataType, Choose the type of residual to be appended. Raw residuals are simply the differences between observation and model prediction, standardized residuals divide each residual by an estimate of its standard deviation (using rstandard()), whereas studentized residuals divide each residual by an unbiased estimate of its standard deviation computed by refitting the model without that observation  (using rstudent()).
#' \itemize{
#'   \item \code{"raw"}
#'   \item \code{"standard"}
#'   \item \code{"student"}
#' }
#' @param steppingMethodCriteriaFEntry, Adds predictor if the F-value (t^2) of the regression coefficient is > x; default is x=3.84.
#' @param steppingMethodCriteriaFRemoval, Removes predictor if the F-value (t^2) of the regression coefficient is < x; default is x=2.71.
#' @param steppingMethodCriteriaPEntry, Adds predictor if the p-value of the regression coefficient < x; default is x=0.05.
#' @param steppingMethodCriteriaPRemoval, Removes predictor if the p-value of the regression coefficient > x; default is x=0.1.
#' @param vovkSellke, Shows the maximum ratio of the likelihood of the obtained p-value under H1 vs the likelihood of the obtained p value under H0. For example, if the two-sided p-value equals .05, the Vovk-Sellke MPR equals 2.46, indicating that this p-value is at most 2.46 times more likely to occur under H1 than under H0.
#'    Defaults to \code{FALSE}.
#' @param weights, The weights used for weighted least squares regression.
RegressionLinear <- function(
          data = NULL,
          version = "1",
          coefficientBootstrap = FALSE,
          coefficientBootstrapSamples = 5000,
          coefficientCi = FALSE,
          coefficientCiLevel = 0.95,
          coefficientEstimate = TRUE,
          collinearityDiagnostic = FALSE,
          collinearityStatistic = FALSE,
          covarianceMatrix = FALSE,
          covarianceRatio = FALSE,
          covariates = list(types = list(), value = list()),
          dependent = list(types = list(), value = ""),
          descriptivePlotCiLevel = 0.95,
          descriptivePlotErrorBar = TRUE,
          descriptivePlotErrorBarType = "ci",
          descriptivePlotHorizontalAxis = list(types = list(), value = ""),
          descriptivePlotScaleGroupingMethod = "sd",
          descriptivePlotScaleGroups = 3,
          descriptivePlotSeparateLines = list(types = list(), value = ""),
          descriptivePlotSeparatePlot = list(types = list(), value = ""),
          descriptives = FALSE,
          dfbetas = FALSE,
          dffits = FALSE,
          equationTable = FALSE,
          fChange = FALSE,
          factors = list(types = list(), value = list()),
          interceptTerm = TRUE,
          leverage = FALSE,
          mahalanobis = FALSE,
          marginalPlot = FALSE,
          marginalPlotCi = FALSE,
          marginalPlotCiLevel = 0.95,
          marginalPlotPredictionInterval = FALSE,
          marginalPlotPredictionIntervalLevel = 0.95,
          method = "enter",
          modelAICBIC = FALSE,
          modelFit = TRUE,
          modelTerms = list(list(components = list(), name = "model0", title = "Model 0"), list(components = list(), name = "model1", title = "Model 1")),
          naAction = "listwise",
          nested = TRUE,
          optimizationPlot = FALSE,
          optimizationPlotCustomParameterValues = list(optionKey = "variable", types = list(), value = list()),
          optimizationPlotCustomParameters = FALSE,
          optimizationSolutionTable = FALSE,
          partAndPartialCorrelation = FALSE,
          partialResidualPlot = FALSE,
          partialResidualPlotCi = FALSE,
          partialResidualPlotCiLevel = 0.95,
          partialResidualPlotPredictionInterval = FALSE,
          partialResidualPlotPredictionIntervalLevel = 0.95,
          plotHeight = 320,
          plotWidth = 480,
          predictionsSavedToData = FALSE,
          predictionsSavedToDataColumn = "",
          qqPlotCi = FALSE,
          qqPlotCiLevel = 0.95,
          quadraticTerms = FALSE,
          rSquaredChange = TRUE,
          residualCasewiseDiagnostic = FALSE,
          residualCasewiseDiagnosticCooksDistanceThreshold = 1,
          residualCasewiseDiagnosticType = "outliersOutside",
          residualCasewiseDiagnosticZThreshold = 3,
          residualDurbinWatson = FALSE,
          residualHistogramPlot = FALSE,
          residualHistogramStandardizedPlot = TRUE,
          residualQqPlot = FALSE,
          residualStatistic = FALSE,
          residualVsCovariatePlot = FALSE,
          residualVsDependentPlot = FALSE,
          residualVsFittedPlot = FALSE,
          residualsSavedToData = FALSE,
          residualsSavedToDataColumn = "",
          residualsSavedToDataType = "raw",
          responseOptimizerGoal = "maximize",
          responseOptimizerLowerBound = 0,
          responseOptimizerManualBounds = FALSE,
          responseOptimizerManualTarget = FALSE,
          responseOptimizerTarget = 0.5,
          responseOptimizerUpperBound = 1,
          responseOptimizerWeight = 1,
          steppingMethodCriteriaFEntry = 3.84,
          steppingMethodCriteriaFRemoval = 2.71,
          steppingMethodCriteriaPEntry = 0.05,
          steppingMethodCriteriaPRemoval = 0.1,
          steppingMethodCriteriaType = "pValue",
          vovkSellke = FALSE,
          weights = list(types = list(), value = "")) {

   defaultArgCalls <- formals(jaspRegression::RegressionLinear)
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

   optionsWithFormula <- c("covariates", "dependent", "descriptivePlotHorizontalAxis", "descriptivePlotScaleGroupingMethod", "descriptivePlotSeparateLines", "descriptivePlotSeparatePlot", "factors", "method", "modelTerms", "optimizationPlotCustomParameterValues", "responseOptimizerGoal", "weights")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspRegression", "RegressionLinear", "RegressionLinear.qml", options, version, TRUE))
}