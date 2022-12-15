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

GeneralizedLinearModel <- function(
          data = NULL,
          version = "0.17",
          formula = NULL,
          isNuisance = NULL,
          coefficientCi = FALSE,
          coefficientCiLevel = 0.95,
          coefficientEstimate = TRUE,
          contrasts = list(),
          cooksDistance = FALSE,
          covarianceRatio = FALSE,
          covariates = list(),
          dependent = "",
          devianceGoodnessOfFit = FALSE,
          devianceResidualQqPlot = FALSE,
          devianceResidualVsFittedPlot = FALSE,
          devianceResidualVsPredictorPlot = FALSE,
          dfbetas = FALSE,
          dffits = FALSE,
          factors = list(),
          family = "bernoulli",
          interceptTerm = TRUE,
          leverage = FALSE,
          link = "logit",
          marginalMeansCi = FALSE,
          marginalMeansCiWidth = 0.95,
          marginalMeansComparison = FALSE,
          marginalMeansComparisonWith = 0,
          marginalMeansContrast = FALSE,
          marginalMeansPAdjustment = "holm",
          marginalMeansResponse = TRUE,
          marginalMeansSd = 1,
          marginalMeansVars = list(),
          modelTerms = list(),
          offset = "",
          otherGlmModel = "multinomialLogistic",
          partialResidualPlot = FALSE,
          pearsonGoodnessOfFit = FALSE,
          pearsonResidualQqPlot = FALSE,
          pearsonResidualVsFittedPlot = FALSE,
          pearsonResidualVsPredictorPlot = FALSE,
          plotHeight = 320,
          plotWidth = 480,
          quantileResidualOutlierTable = FALSE,
          quantileResidualOutlierTableTopN = 3,
          quantileResidualQqPlot = FALSE,
          quantileResidualVsFittedPlot = FALSE,
          quantileResidualVsPredictorPlot = FALSE,
          seed = 1,
          setSeed = FALSE,
          standardizedResidualOutlierTable = FALSE,
          standardizedResidualOutlierTableTopN = 3,
          studentizedResidualOutlierTable = FALSE,
          studentizedResidualOutlierTableTopN = 3,
          tolerance = FALSE,
          vif = FALSE,
          weights = "",
          workingResponseVsLinearPredictorPlot = FALSE) {

   defaultArgCalls <- formals(jaspRegression::GeneralizedLinearModel)
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

   optionsWithFormula <- c("isNuisance", "contrasts", "covariates", "dependent", "factors", "family", "marginalMeansPAdjustment", "marginalMeansVars", "modelTerms", "offset", "otherGlmModel", "weights")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspRegression::GeneralizedLinearModel", data, options, version))
}
