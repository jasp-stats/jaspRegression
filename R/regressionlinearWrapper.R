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

RegressionLinear <- function(
          data = NULL,
          version = "0.18.2",
          formula = NULL,
          isNuisance = NULL,
          coefficientBootstrap = FALSE,
          coefficientBootstrapSamples = 5000,
          coefficientCi = FALSE,
          coefficientCiLevel = 0.95,
          coefficientEstimate = TRUE,
          collinearityDiagnostic = FALSE,
          covarianceMatrix = FALSE,
          covariates = list(),
          dependent = "",
          descriptives = FALSE,
          factors = list(),
          interceptTerm = TRUE,
          marginalPlot = FALSE,
          marginalPlotCi = FALSE,
          marginalPlotCiLevel = 0.95,
          marginalPlotPredictionInterval = FALSE,
          marginalPlotPredictionIntervalLevel = 0.95,
          method = "enter",
          modelFit = TRUE,
          modelTerms = list(),
          naAction = "listwise",
          partAndPartialCorrelation = FALSE,
          partialResidualPlot = FALSE,
          partialResidualPlotCi = FALSE,
          partialResidualPlotCiLevel = 0.95,
          partialResidualPlotPredictionInterval = FALSE,
          partialResidualPlotPredictionIntervalLevel = 0.95,
          plotHeight = 320,
          plotWidth = 480,
          rSquaredChange = FALSE,
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
          steppingMethodCriteriaFEntry = 3.84,
          steppingMethodCriteriaFRemoval = 2.71,
          steppingMethodCriteriaPEntry = 0.05,
          steppingMethodCriteriaPRemoval = 0.1,
          steppingMethodCriteriaType = "pValue",
          vovkSellke = FALSE,
          weights = "") {

   defaultArgCalls <- formals(jaspRegression::RegressionLinear)
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

   return(jaspBase::runWrappedAnalysis("jaspRegression::RegressionLinear", data, options, version))
}