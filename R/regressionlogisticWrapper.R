#
# Copyright (C) 2013-2022 University of Amsterdam
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

# This is a generated file. Don't change it

RegressionLogistic <- function(
          data = NULL,
          version = "0.19",
          formula = NULL,
          isNuisance = NULL,
          accuracy = FALSE,
          auc = FALSE,
          brierScore = FALSE,
          coefficientBootstrap = FALSE,
          coefficientBootstrapSamples = 5000,
          coefficientCi = FALSE,
          coefficientCiAsOddsRatio = FALSE,
          coefficientCiLevel = 0.95,
          coefficientEstimate = TRUE,
          coefficientStandardized = FALSE,
          conditionalEstimatePlot = FALSE,
          conditionalEstimatePlotCi = 0.95,
          conditionalEstimatePlotPoints = FALSE,
          confusionMatrix = FALSE,
          covarianceRatio = FALSE,
          covariates = list(),
          dependent = "",
          descriptives = FALSE,
          dfbetas = FALSE,
          dffits = FALSE,
          fMeasure = FALSE,
          factors = list(),
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
          oddsRatio = FALSE,
          plotHeight = 320,
          plotWidth = 480,
          precision = FALSE,
          precisionRecallPlot = FALSE,
          precisionRecallPlotCutoffLabel = FALSE,
          precisionRecallPlotCutoffStep = 0.2,
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
          robustSe = FALSE,
          rocPlot = FALSE,
          rocPlotCutoffLabel = FALSE,
          rocPlotCutoffStep = 0.2,
          sensitivity = FALSE,
          specificity = FALSE,
          squaredPearsonResidualVsFittedPlot = FALSE,
          vovkSellke = FALSE,
          weights = "") {

   defaultArgCalls <- formals(jaspRegression::RegressionLogistic)
   defaultArgs <- lapply(defaultArgCalls, eval)
   options <- as.list(match.call())[-1L]
   options <- lapply(options, eval)
   defaults <- setdiff(names(defaultArgs), names(options))
   options[defaults] <- defaultArgs[defaults]
   options[["data"]] <- NULL
   options[["version"]] <- NULL

   if (!is.null(formula)) {
      if (!inherits(formula, "formula")) {
         formula <- as.formula(formula)
      }
      options$formula <- jaspBase::jaspFormula(formula, data)
   }

   optionsWithFormula <- c("isNuisance", "covariates", "dependent", "factors", "method", "modelTerms", "weights")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspRegression::RegressionLogistic", data, options, version))
}
