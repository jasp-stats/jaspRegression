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

#' Logistic Regression
#'
#' Logistic regression allows the user to model a linear relationship between one or more explanatory variable/s and a categorical dependent variable.
#' ## Assumptions
#' - The dependent variables are categorical
#' - The dependent variable is linearly related to all predictors and the effects of the predictors are additive.
#' - The assumption of homoscedasticity is met. Homoscedasticity entails that the error variance of each predictor is constant across all values of that predictor.
#' - The residuals are uncorrelated with each other.
#' - The residuals are normally distributed with a mean zero.
#' - The covariate and the experiment effect are independent.
#'
#' @param accuracy, How often the model's prediction match the actual outcomes
#'    Defaults to \code{FALSE}.
#' @param auc, Area Under the Curve.
#'    Defaults to \code{FALSE}.
#' @param brierScore, Another measure of the accuracy of predictions. Based on the mean squared difference between predicted probabilities and the binary outcome. Lower scores mean more accuracy.
#'    Defaults to \code{FALSE}.
#' @param coefficientBootstrap, By selecting this option, bootstrapped estimation is applied. By default, the number of replications is set to 5000. This can be changed into the desired number.
#'    Defaults to \code{FALSE}.
#' @param coefficientCi, Coverage of the confidence intervals in percentages. The default value is 95.
#'    Defaults to \code{FALSE}.
#' @param coefficientEstimate, Coefficient estimates, standard errors, z-values, and their corresponding p-values.
#'    Defaults to \code{TRUE}.
#' @param coefficientStandardized, Standardized estimates represent estimates were the predictors are standardized (X-standardization).
#'    Defaults to \code{FALSE}.
#' @param conditionalEstimatePlot, The plots are conditional in the sense that they display the probability of the categorical dependent variable for all levels of the predictor variables given the reference level of all other factors.
#'    Defaults to \code{FALSE}.
#' @param confusionMatrix, The confusion matrix indicates how well the model predicts the outcomes. The table is showing actual versus predicted outcomes and can be used to determine the accuracy of the model.
#'    Defaults to \code{FALSE}.
#' @param covarianceRatio, The degree to which a case influences the variance of the regression parameters. Tells when the covariance ratio is greater than 3 * k/(n-k).
#'    Defaults to \code{FALSE}.
#' @param covariates,  In this box the variable that is the covariate can be selected. Covariates are continuous variables that have an influence on the dependent variable but are not part of the experimental manipulation.
#' @param dependent, The variable of interest. This is also called the outcome variable. In case of multiple dependent variables, specify the order in which the predictors are entered into the model i.e., hierarchical regression analysis. A block of one or more predictors represents one step in the hierarchy.
#' @param descriptives, The levels of the dependent variable/s and the number of observations per level.
#'    Defaults to \code{FALSE}.
#' @param dfbetas, The difference between a parameter estimated using all cases and estimated when one case is excluded. tells when the absolute value of DFBETAS is greater than 1.
#'    Defaults to \code{FALSE}.
#' @param dffits, The difference between the predicted value for a case when the model is estimated including or excluding that case. tells when the absolute value of DFFITS is greater than 3 * sqrt(k/(n-k)) 
#'    Defaults to \code{FALSE}.
#' @param fMeasure, This is based on the amount of systematic variance divided by the amount of unsystematic variance i.e., mean squares for the model / the residual mean squares
#'    Defaults to \code{FALSE}.
#' @param factors, The variables that are manipulated/define the different groups. These are also called the independent variables.
#' @param hMeasure, A more theoretically robust and advanced alternative to the AUC.
#'    Defaults to \code{FALSE}.
#' @param independentVsPredictedPlot, Plots the model predictions against each independent variable.
#'    Defaults to \code{FALSE}.
#' @param independentVsPredictedPlotIncludeInteractions, also add every two-way interaction
#'    Defaults to \code{TRUE}.
#' @param independentVsPredictedPlotUseLogit, plot predicted probabilities on the logit scale, to ensure a linear relation
#'    Defaults to \code{TRUE}.
#' @param interceptTerm, Ticking this box will add a coefficient estimate of the intercept as well. This corresponds to the first level for the independent variable
#'    Defaults to \code{TRUE}.
#' @param leverage, The influence of the observed value of the outcome variable over the predicted values. Tells when the leverages are greater than 3 * k/n.
#'    Defaults to \code{FALSE}.
#' @param mahalanobis, Measures the distance of cases from the mean(s) of the predictor variable(s)
#'    Defaults to \code{FALSE}.
#' @param modelTerms, The independent variables and covariates included in the model. By default, all the main effects and interaction effects of the specified independent variables, and the covariates are included in the model.
#' @param multicollinearity, Display Tolerance and Variance Inflation Factor for each predictor in the model to assess multicollinearity.
#'    Defaults to \code{FALSE}.
#' @param oddsRatio, Odds ratio is an indicator of the change in odds resulting from a unit change in the predictor.
#'    Defaults to \code{TRUE}.
#' @param precision, Precision describes the proportion of true positives to all positives. Also called the positive predictive value.
#'    Defaults to \code{FALSE}.
#' @param precisionRecallPlot, Displays the Positive predicitve value on the y axis vs the true positive rate/sensitivity on the x axis. Useful for imbalanced datadets. The cutoff step determines the threshold at which an observation is classified as positive or negative. 
#'    Defaults to \code{FALSE}.
#' @param predictionsSavedToData, Save the predictions of the most complex model as a new column in the data file
#'    Defaults to \code{FALSE}.
#' @param residualCasewiseDiagnosticType, Casewise and sumamrized diagnostics for the residuals. There is an option to display diagnostics for cases where the absolut value of the standardized residual is larger than x (defaultis x=3). There is another option to display diagnostics for cases where the value of Cook’s distance is larger than x (default is x = 1.). And there is also an option to display diagnostics for all cases.
#' \itemize{
#'   \item \code{"allCases"}
#'   \item \code{"outliersOutside"}
#'   \item \code{"cooksDistance"}
#' }
#' @param residualStatistic, Display descriptive statistics of the residuals and predicted values
#'    Defaults to \code{FALSE}.
#' @param residualVsFittedPlot, Scatterplot of the values of the residuals against the predicted values.
#'    Defaults to \code{FALSE}.
#' @param residualVsPredictorPlot, Scatterplot for every independent variable and covariate of the residuals and the levels of the variable of interest.
#'    Defaults to \code{FALSE}.
#' @param residualsSavedToData, Save the residuals of the most complex model as a new column in the data file.
#'    Defaults to \code{FALSE}.
#' @param residualsSavedToDataType, Choose the type of residual to be appended. Raw residuals are simply the differences between observation and model prediction, standardized residuals divide each residual by an estimate of its standard deviation (using rstandard()), whereas studentized residuals divide each residual by an unbiased estimate of its standard deviation computed by refitting the model without that observation  (using rstudent()).
#' \itemize{
#'   \item \code{"student"}
#'   \item \code{"raw"}
#'   \item \code{"standard"}
#' }
#' @param robustSe, this option controls for errors that are not independent and identically distributed. The use of robust standard errors will not change the coefficient estimates. If this option is not selected the normal standard error will be computed.
#'    Defaults to \code{FALSE}.
#' @param rocPlot, Plots performance of the model by plotting the true positive rate to the false positive rate. The cutoff step determines the thereshold at which an observation is classified as positive or a negative.
#'    Defaults to \code{FALSE}.
#' @param sensitivity, Sensitivity describes the proportion of true positives.
#'    Defaults to \code{FALSE}.
#' @param specificity, Specificity describes the proportion of true negatives.
#'    Defaults to \code{FALSE}.
#' @param squaredPearsonResidualVsFittedPlot, With the Squared Pearson residuals plot one can check for overdispersion of the model. Overdispersion indicates that the actual data show greater variability than the model has predicted.
#'    Defaults to \code{FALSE}.
#' @param vovkSellke, Shows the maximum ratio of the lieklihood of the obtained p value under H1 vs the likelihood of the obtained p value under H0. For example, if the two-sided p-value equals .05, the Vovk-Sellke MPR equals 2.46, indicating that this p-value is at most 2.46 times more likely to occur under H1 than under H0
#'    Defaults to \code{FALSE}.
#' @param weights, The weights used for weighted least square regression.
RegressionLogistic <- function(
          data = NULL,
          version = "0.95",
          accuracy = FALSE,
          auc = FALSE,
          brierScore = FALSE,
          coefficientBootstrap = FALSE,
          coefficientBootstrapSamples = 5000,
          coefficientCi = FALSE,
          coefficientCiAsOddsRatio = TRUE,
          coefficientCiLevel = 0.95,
          coefficientEstimate = TRUE,
          coefficientStandardized = FALSE,
          conditionalEstimatePlot = FALSE,
          conditionalEstimatePlotCi = 0.95,
          conditionalEstimatePlotPoints = FALSE,
          confusionMatrix = FALSE,
          covarianceRatio = FALSE,
          covariates = list(types = list(), value = list()),
          dependent = list(types = list(), value = ""),
          descriptives = FALSE,
          dfbetas = FALSE,
          dffits = FALSE,
          fMeasure = FALSE,
          factors = list(types = list(), value = list()),
          hMeasure = FALSE,
          independentVsPredictedPlot = FALSE,
          independentVsPredictedPlotIncludeInteractions = TRUE,
          independentVsPredictedPlotUseLogit = TRUE,
          interceptTerm = TRUE,
          leverage = FALSE,
          mahalanobis = FALSE,
          method = "enter",
          modelTerms = list(list(components = list(), name = "model0", title = "Model 0"), list(components = list(), name = "model1", title = "Model 1")),
          multicollinearity = FALSE,
          nested = TRUE,
          oddsRatio = TRUE,
          plotHeight = 320,
          plotWidth = 480,
          precision = FALSE,
          precisionRecallPlot = FALSE,
          precisionRecallPlotCutoffLabel = FALSE,
          precisionRecallPlotCutoffStep = 0.2,
          predictionsSavedToData = FALSE,
          predictionsSavedToDataColumn = "",
          residualCasewiseDiagnostic = FALSE,
          residualCasewiseDiagnosticCooksDistanceThreshold = 1,
          residualCasewiseDiagnosticType = "outliersOutside",
          residualCasewiseDiagnosticZThreshold = 3,
          residualStatistic = FALSE,
          residualType = "deviance",
          residualVsFittedPlot = FALSE,
          residualVsPredictorPlot = FALSE,
          residualsSavedToData = FALSE,
          residualsSavedToDataColumn = "",
          residualsSavedToDataType = "raw",
          robustSe = FALSE,
          rocPlot = FALSE,
          rocPlotCutoffLabel = FALSE,
          rocPlotCutoffStep = 0.2,
          sensitivity = FALSE,
          specificity = FALSE,
          squaredPearsonResidualVsFittedPlot = FALSE,
          vovkSellke = FALSE,
          weights = list(types = list(), value = "")) {

   defaultArgCalls <- formals(jaspRegression::RegressionLogistic)
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

   optionsWithFormula <- c("covariates", "dependent", "factors", "method", "modelTerms", "weights")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspRegression", "RegressionLogistic", "RegressionLogistic.qml", options, version, TRUE))
}