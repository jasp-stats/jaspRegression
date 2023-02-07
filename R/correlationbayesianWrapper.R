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

CorrelationBayesian <- function(
          data = NULL,
          version = "0.17.1",
          alternative = "twoSided",
          bayesFactorReport = TRUE,
          bayesFactorType = "BF10",
          bfRobustnessPlot = FALSE,
          bfRobustnessPlotAdditionalInfo = TRUE,
          bfSequentialPlot = FALSE,
          bfSequentialPlotAdditionalInfo = TRUE,
          ci = FALSE,
          ciLevel = 0.95,
          kendall = FALSE,
          matrixPlot = FALSE,
          matrixPlotDensity = FALSE,
          matrixPlotPosterior = FALSE,
          naAction = "pairwise",
          pairsMethod = "pearson",
          pairwiseDisplay = FALSE,
          pearson = TRUE,
          plotHeight = 320,
          plotWidth = 480,
          posteriorMedian = FALSE,
          priorPosteriorPlot = FALSE,
          priorPosteriorPlotAdditionalEstimationInfo = TRUE,
          priorPosteriorPlotAdditionalTestingInfo = TRUE,
          priorWidth = "1.0",
          sampleSize = FALSE,
          scatterPlot = TRUE,
          scatterPlotAdditionalInfo = FALSE,
          seed = 1,
          setSeed = FALSE,
          spearman = FALSE,
          supportCorrelationFlagged = FALSE,
          variablePairs = list(),
          variables = list()) {

   defaultArgCalls <- formals(jaspRegression::CorrelationBayesian)
   defaultArgs <- lapply(defaultArgCalls, eval)
   options <- as.list(match.call())[-1L]
   options <- lapply(options, eval)
   defaults <- setdiff(names(defaultArgs), names(options))
   options[defaults] <- defaultArgs[defaults]
   options[["data"]] <- NULL
   options[["version"]] <- NULL

   optionsWithFormula <- c("variablePairs", "variables")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspRegression::CorrelationBayesian", data, options, version))
}