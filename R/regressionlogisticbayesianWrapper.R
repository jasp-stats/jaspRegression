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

RegressionLogisticBayesian <- function(
          data = NULL,
          version = "0.18.2",
          bayesFactorOrder = "bestModelTop",
          bayesFactorType = "BF10",
          bernoulliParam = 0.5,
          betaBinomialParamA = 1,
          betaBinomialParamB = 1,
          castilloParamU = 1,
          cchPriorAlpha = 0.5,
          cchPriorBeta = 2,
          cchPriorS = 0,
          covariates = list(),
          dependent = "",
          descriptives = FALSE,
          effectsType = "allModels",
          factors = list(),
          gPriorAlpha = 3,
          inclusionProbabilitiesPlot = FALSE,
          logPosteriorOddsPlot = FALSE,
          marginalPosteriorPlot = FALSE,
          modelComplexityPlot = FALSE,
          modelPrior = "betaBinomial",
          modelProbabilitiesPlot = FALSE,
          modelTerms = list(),
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
          weights = "",
          wilsonParamLambda = 1) {

   defaultArgCalls <- formals(jaspRegression::RegressionLogisticBayesian)
   defaultArgs <- lapply(defaultArgCalls, eval)
   options <- as.list(match.call())[-1L]
   options <- lapply(options, eval)
   defaults <- setdiff(names(defaultArgs), names(options))
   options[defaults] <- defaultArgs[defaults]
   options[["data"]] <- NULL
   options[["version"]] <- NULL

   optionsWithFormula <- c("covariates", "dependent", "factors", "modelTerms", "summaryType", "weights")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspRegression::RegressionLogisticBayesian", data, options, version))
}