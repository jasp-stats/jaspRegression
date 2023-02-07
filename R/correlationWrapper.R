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

Correlation <- function(
          data = NULL,
          version = "0.17",
          alternative = "twoSided",
          assumptionCheckMultivariateEnergy = FALSE,
          assumptionCheckMultivariateMardia = FALSE,
          assumptionCheckMultivariateRoyston = FALSE,
          assumptionCheckMultivariateShapiro = FALSE,
          assumptionCheckPairwiseEnergy = FALSE,
          assumptionCheckPairwiseMardia = FALSE,
          assumptionCheckPairwiseRoyston = FALSE,
          assumptionCheckPairwiseShapiro = FALSE,
          ci = FALSE,
          ciBootstrap = FALSE,
          ciBootstrapSamples = 1000,
          ciLevel = 0.95,
          crossProducts = FALSE,
          effectSize = FALSE,
          heatmapPlot = FALSE,
          kendallsTauB = FALSE,
          meansAndSd = FALSE,
          naAction = "pairwise",
          pairwiseDisplay = FALSE,
          partialOutVariables = list(),
          pearson = TRUE,
          plotHeight = 320,
          plotWidth = 480,
          sampleSize = FALSE,
          scatterPlot = FALSE,
          scatterPlotCi = FALSE,
          scatterPlotCiLevel = 0.95,
          scatterPlotDensity = FALSE,
          scatterPlotPredictionInterval = FALSE,
          scatterPlotPredictionIntervalLevel = 0.95,
          scatterPlotStatistic = FALSE,
          significanceFlagged = FALSE,
          significanceReport = TRUE,
          spearman = FALSE,
          variables = list(),
          vovkSellke = FALSE) {

   defaultArgCalls <- formals(jaspRegression::Correlation)
   defaultArgs <- lapply(defaultArgCalls, eval)
   options <- as.list(match.call())[-1L]
   options <- lapply(options, eval)
   defaults <- setdiff(names(defaultArgs), names(options))
   options[defaults] <- defaultArgs[defaults]
   options[["data"]] <- NULL
   options[["version"]] <- NULL

   optionsWithFormula <- c("partialOutVariables", "variables")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspRegression::Correlation", data, options, version))
}