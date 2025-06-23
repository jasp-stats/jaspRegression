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

#' Correlation
#'
#' The Correlation analysis allows estimation of the population correlation, as well as testing the null hypothesis that the population correlation between pairs of variables equals 0. All possible pairs of the specified variables are analyzed.
#' ## Assumptions (Pearson's rho)
#' - The variables are both continuous
#' - The data are a random sample from the population
#' - The pairs of variables follow a bivariate normal distribution in the population
#' - The relationship between the variables is linear
#' ## Assumptions (Spearman's rho and Kendall's tau)
#' - Ordinal or continuous variables
#' - The data are a random sample from the population
#' - The relationship between the pairs of variables is monotonic
#'
#' @param ci, Confidence intervals for the population correlation (only available for the Pearson correlation). By default is set at 95% but the percentage can be changed. There is also an option to set up a bootstrap estimation, set at 1000 by default with the option to change it into the desired number.
#'    Defaults to \code{FALSE}.
#' @param covariance,  The covariance between each pair of variables.
#'    Defaults to \code{FALSE}.
#' @param effectSize, The Fisher transformed effect size with standard error.
#'    Defaults to \code{FALSE}.
#' @param heatmapPlot, Display a correlation heatmap for Pearson, Spearman, and Kendall's tau B coefficients separately.
#'    Defaults to \code{FALSE}.
#' @param kendallsTauB, Kendall's tau-b rank-order correlation coefficient to quantify the monotonic association between two variables by comparing concordant and non-concordant pairs. Use when data is not normally distributed
#'    Defaults to \code{FALSE}.
#' @param pairwiseDisplay, Display a table where one row corresponds to one pair of the specified variables, and the scatter plots are shown individually for each pair. If unticked, the results are presented in matrix format, with variable names in the columns and rows.
#'    Defaults to \code{TRUE}.
#' @param partialOutVariables, Variables to partial out in order to compute partial correlations.
#' @param pearson, Pearson's product moment correlation coefficient. This is the famous r coefficient
#'    Defaults to \code{TRUE}.
#' @param sampleSize, The number of complete observations for a given pair of variables.
#'    Defaults to \code{FALSE}.
#' @param scatterPlot, Display a scatter plots for each possible combination of the selected variables. In a matrix format, these are placed above the diagonal.
#'    Defaults to \code{FALSE}.
#' @param scatterPlotDensity, Display histogram and the corresponding density plot for each variable. In a matrix format, these are placed on the diagonal
#'    Defaults to \code{FALSE}.
#' @param scatterPlotStatistic, Display the correlation coefficient(s) in the plot. This option also adds the x% confidence interval(s) as specified in the Confidence Intervals option.
#'    Defaults to \code{FALSE}.
#' @param significanceFlagged, Mark statistically significant correlations.
#'    Defaults to \code{FALSE}.
#' @param significanceReport, Display the p-value corresponding to the observed correlation.
#'    Defaults to \code{TRUE}.
#' @param spearman, Spearman's rank-order correlation coefficient to quantify the monotonic association between two variables by ranking the observations. Use when data is not normally distributed
#'    Defaults to \code{FALSE}.
#' @param variables, Variables for which to compute the correlation coefficient
#' @param vovkSellke, Shows the maximum ratio of the lieklihood of the obtained p value under H1 vs the likelihood of the obtained p value under H0. For example, if the two-sided p-value equals .05, the Vovk-Sellke MPR equals 2.46, indicating that this p-value is at most 2.46 times more likely to occur under H1 than under H0
#'    Defaults to \code{FALSE}.
Correlation <- function(
          data = NULL,
          version = "0.95",
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
          covariance = FALSE,
          crossProducts = FALSE,
          effectSize = FALSE,
          heatmapPlot = FALSE,
          kendallsTauB = FALSE,
          meansAndSd = FALSE,
          naAction = "pairwise",
          pairwiseDisplay = TRUE,
          partialOutVariables = list(types = list(), value = list()),
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
          variables = list(types = list(), value = list()),
          vovkSellke = FALSE) {

   defaultArgCalls <- formals(jaspRegression::Correlation)
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

   optionsWithFormula <- c("partialOutVariables", "variables")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspRegression", "Correlation", "Correlation.qml", options, version, TRUE))
}