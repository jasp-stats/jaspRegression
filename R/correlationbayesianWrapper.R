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

#' Bayesian Correlation
#'
#' The Bayesian Correlation analysis allows for the estimation of the population correlation, as well as the comparison of hypotheses. The three comparisons are (i) between the null hypothesis (H0) that the correlation between pairs of variables equal 0, and the alternative hypothesis (H1) that the population correlation takes its value between -1 and 1; (ii) between H0 and the alternative hypothesis (H+) that the population correlation is positive; and (iii) between H0 and the alternative hypothesis (H-) that the population correlation is negative. All possible pairs of the specified variables are analyzed.
#' ## Assumptions (Pearson's rho)
#' - The variables are both continuous
#' - The data are a random sample from the population
#' The pairs of variables follow a bivariate normal distribution in the population
#'  - The relationship between the variables is linear
#' ## Assumptions (Kendall's tau)
#' - Ordinal or continuous variables
#' - The data are a random sample from the population
#' - The relationship between the pairs of variables is monotonic
#'
#' @param bayesFactorReport, Report Bayes factor for each test.
#'    Defaults to \code{TRUE}.
#' @param bfRobustnessPlot, Displays the Bayes factor as a function of the width of the stretched beta prior on the correlation for each specified pair of variables. The width of the kappa prior is varied between 0 and 2.
#'    Defaults to \code{FALSE}.
#' @param bfRobustnessPlotAdditionalInfo, Adds the Bayes factor computed with the user-defined prior and the maximum obtainable Bayes factor.
#'    Defaults to \code{TRUE}.
#' @param bfSequentialPlot, Displays the development of the Bayes factor as the data come in using the user-defined prior for each specified pair of variables.
#'    Defaults to \code{FALSE}.
#' @param bfSequentialPlotAdditionalInfo, Adds the Bayes factor computed with the user-defined prior; adds a probability wheel depicting the odds of the data under the null vs. alternative hypothesis; shows the decisiveness of the evidence in terms of Jeffreys' (1961) evidence categories.
#'    Defaults to \code{TRUE}.
#' @param ci, Display central 95% credible intervals for the correlation coefficient. The percentage can be changed.
#'    Defaults to \code{FALSE}.
#' @param kendall, Kendall's tau-b rank-order correlation coefficient to quantify the monotonic association between two variables by comparing concordant and non-concordant pairs. Use when data is not normally distributed
#'    Defaults to \code{FALSE}.
#' @param linearityTest, Compute a test for the linearity of the relationship between the two variables by comparing a linear model to a quadratic model.
#'    Defaults to \code{FALSE}.
#' @param matrixPlot, Display a grid of scatterplots for each possible combination of the selected variables. These are placed above the diagonal.
#'    Defaults to \code{FALSE}.
#' @param matrixPlotDensity, Display histogram and the corresponding density plot for each variable. These are placed on the diagonal.
#'    Defaults to \code{FALSE}.
#' @param matrixPlotPosterior, Display posterior distribution of the correlation coefficient for each possible combination of the selected variables. These are placed below the diagonal.
#'    Defaults to \code{FALSE}.
#' @param pairwiseDisplay, Display a table where one row corresponds to one pair of the specified variables. If unticked, the results are presented in matrix format, with variable names in the columns and rows.
#'    Defaults to \code{TRUE}.
#' @param pearson, Pearson's product moment correlation coefficient. This is the famous r value
#'    Defaults to \code{TRUE}.
#' @param priorPosteriorPlot, Displays the prior and posterior distribution of the correlation under the alternative hypothesis for each specified pair of variables.
#'    Defaults to \code{FALSE}.
#' @param priorPosteriorPlotAdditionalEstimationInfo, Adds the median and the 95% credible interval of the posterior distribution of the effect size.
#'    Defaults to \code{TRUE}.
#' @param priorPosteriorPlotAdditionalTestingInfo, Adds the Bayes factor computed with the user-defined prior; adds a probability wheel depicting the odds of the data under the null vs. alternative hypothesis (Assuming null and alternative had equal probability a priori).
#'    Defaults to \code{TRUE}.
#' @param sampleSize, Report the sample size for each test.
#'    Defaults to \code{FALSE}.
#' @param scatterPlot, Displays scatterplots for each specified pair of variables.
#'    Defaults to \code{TRUE}.
#' @param supportCorrelationFlagged, Indicate which correlation coefficients yield Bayes factors greater than 10, 30, and 100.
#'    Defaults to \code{FALSE}.
#' @param variables, - Assigned variables: Variables for which to compute the correlation coefficient
CorrelationBayesian <- function(
          data = NULL,
          version = "0.95",
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
          linearityTest = FALSE,
          matrixPlot = FALSE,
          matrixPlotDensity = FALSE,
          matrixPlotPosterior = FALSE,
          naAction = "pairwise",
          pairsMethod = "pearson",
          pairwiseDisplay = TRUE,
          pearson = TRUE,
          plotHeight = 320,
          plotWidth = 480,
          posteriorMedian = FALSE,
          priorPosteriorPlot = FALSE,
          priorPosteriorPlotAdditionalEstimationInfo = TRUE,
          priorPosteriorPlotAdditionalTestingInfo = TRUE,
          priorWidth = 1,
          sampleSize = FALSE,
          scatterPlot = TRUE,
          scatterPlotAdditionalInfo = FALSE,
          seed = 1,
          setSeed = FALSE,
          spearman = FALSE,
          supportCorrelationFlagged = FALSE,
          variablePairs = list(),
          variables = list(types = list(), value = list())) {

   defaultArgCalls <- formals(jaspRegression::CorrelationBayesian)
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

   optionsWithFormula <- c("variablePairs", "variables")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspRegression", "CorrelationBayesian", "CorrelationBayesian.qml", options, version, TRUE))
}