//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//
import QtQuick
import QtQuick.Layouts
import JASP
import JASP.Controls

Form
{
	info: qsTr("The Bayesian Correlation analysis allows for the estimation of the population correlation, as well as the comparison of hypotheses. The three comparisons are (i) between the null hypothesis (H0) that the correlation between pairs of variables equals 0, and the alternative hypothesis (H1) that the population correlation takes its value between -1 and 1; (ii) between H0 and the alternative hypothesis (H+) that the population correlation is positive; and (iii) between H0 and the alternative hypothesis (H-) that the population correlation is negative. All possible pairs of the specified variables are analyzed.\n") +
	"## " + qsTr("Assumptions (Pearson's rho)") + "\n" + "- The variables are both continuous.\n" + "- The data are a random sample from the population.\n" + "- The pairs of variables follow a bivariate normal distribution in the population.\n" + " - The relationship between the variables is linear.\n" + "## " + qsTr("Assumptions (Kendall's tau)") +
    "\n" + "- Ordinal or continuous variables.\n" + "- The data are a random sample from the population.\n" + "- The relationship between the pairs of variables is monotonic."

	VariablesForm
	{
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		infoLabel: qsTr("Input")
		AvailableVariablesList	{	name: "allVariablesList" }
		AssignedVariablesList	{	name: "variables"; info: qsTr("Variables for which to compute the correlation coefficient.") ; allowedColumns: ["scale"]; minNumericLevels: 2 }
	}

	Group
	{
		title: qsTr("Population Correlation Coefficient")
		CheckBox {	name: "pearson";		label: qsTr("Pearson's rho"); info: qsTr("Pearson's product moment correlation coefficient. This is the famous r value.")	;checked: true	}
		CheckBox {	name: "spearman";		label: qsTr("Spearman's rho"); debug: true		}
		CheckBox {	name: "kendall";		label: qsTr("Kendall's tau-b"); info: qsTr("Kendall's tau-b rank-order correlation coefficient to quantify the monotonic association between two variables by comparing concordant and non-concordant pairs. Use when the data are not normally distributed.")				}
	}

	Group
	{
		title: qsTr("Additional Options")
        CheckBox {	name: "pairwiseDisplay";                label: qsTr("Display pairwise table") ; checked: true; info: qsTr("Displays a table where one row corresponds to one pair of the specified variables. If unticked, the results are presented in matrix format, with variable names in the columns and rows.")					}
        CheckBox {	name: "bayesFactorReport";              label: qsTr("Report Bayes factors") ; 	checked: true; info: qsTr("Reports Bayes factor for each test.")}
        CheckBox {	name: "supportCorrelationFlagged";		label: qsTr("Flag supported correlations"); info: qsTr("Indicates which correlation coefficients yield Bayes factors greater than 10, 30, and 100.")				}
        CheckBox {	name: "sampleSize";                     label: qsTr("Sample size"); info: qsTr("Reports the sample size for each test.")								}
        CheckBox {	name: "posteriorMedian";                label: qsTr("Posterior median"); debug: true			}
		CheckBox
		{
			name: "ci"; 
			label: qsTr("Credible intervals"); 
			info: qsTr("Displays central 95% credible intervals for the correlation coefficient. The percentage can be changed.")
            CIField { name: "ciLevel";	label: qsTr("Interval") }
		}
		CheckBox
		{
			name:  "linearityTest"
			label: qsTr("Linearity test")
			info:  qsTr("Compute a test for the linearity of the relationship between the two variables by comparing a linear model to a quadratic model.")
		}
	}

	RadioButtonGroup
	{
		id:		alternative
		name:	"alternative"
		title:	qsTr("Alt. Hypothesis")
        RadioButton {	value: "twoSided";		label: qsTr("Correlated"); info: qsTr("Two-sided alternative hypothesis that the population correlation does not equal 0."); checked: true	}
		RadioButton {	value: "greater";		label: qsTr("Correlated positively"); info: qsTr("One-sided alternative hypothesis that the population correlation is greater than 0.")		}
		RadioButton {	value: "less";			label: qsTr("Correlated negatively"); info: qsTr("One-sided alternative hypothesis that the population correlation is lower than 0.")		}
	}

	Group
	{
		title: qsTr("Plots")
		CheckBox
		{
                        name: "matrixPlot";				label: qsTr("Correlation matrix"); info: qsTr("Displays a grid of scatterplots for each possible combination of the selected variables. These are placed above the diagonal.")
            CheckBox {	name: "matrixPlotDensity";      label: qsTr("Densities for variables"); info: qsTr("Displays a histogram and the corresponding density plot for each variable. These are placed on the diagonal.")	}
            CheckBox {	name: "matrixPlotPosterior";	label: qsTr("Posteriors under H\u2081"); info: qsTr("Displays posterior distribution of the correlation coefficient for each possible combination of the selected variables. These are placed below the diagonal.")	}
		}
	}

	BayesFactorType { correlated: alternative.value }

	Group
	{
		title: qsTr("Prior"); info: qsTr("Stretched beta prior width: Width of the scaled beta distribution on the correlation under the alternative hypothesis; default is 1. The lower this value, the more concentrated the prior density is around 0. Value must be between 0 and 2.")
        FormulaField { name: "priorWidth"; label: qsTr("Stretched beta prior width"); defaultValue: "1.0"; min: 0.003; max: 2}
	}

	Section
	{
		title: qsTr("Plot Individual Pairs")
		VariablesForm
		{
			preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
			AvailableVariablesList		{	name: "allVariablesList2";	source:"variables" }
			AssignedPairsVariablesList	{	name:  "variablePairs";		allowedColumns: ["scale", "ordinal"] }
		}

		RadioButtonGroup
		{
			name: "pairsMethod"
			title: qsTr("Correlation coefficient to plot")
			RadioButton {	value: "pearson";		label: qsTr("Pearson's rho"); info: qsTr("Pearson's product moment correlation coefficient."); checked:	true	}
			RadioButton {	value: "spearman";		label: qsTr("Spearman's rho"); debug:		true	}
			RadioButton {	value: "kendall";		label: qsTr("Kendall's tau"); info: qsTr("Kendall's tau-b rank-order correlation coefficient to quantify the monotonic association between two variables by comparing concordant and non-concordant pairs.")						}
		}

		CheckBox
		{
                        name: "scatterPlot";		label: qsTr("Scatterplot");	info: qsTr("Displays scatterplots for each specified pair of variables.")	;	checked: true
            CheckBox {	name: "scatterPlotAdditionalInfo";	label: qsTr("Robustness check");	debug: true }

		}
		CheckBox
		{
                        name: "priorPosteriorPlot";						label: qsTr("Prior and posterior"); info: qsTr("Displays the prior and posterior distribution of the correlation under the alternative hypothesis for each specified pair of variables.")
            CheckBox {	name: "priorPosteriorPlotAdditionalEstimationInfo";	label: qsTr("Estimation info");	info: qsTr("Adds the median and the 95% credible interval of the posterior distribution of the effect size.")	;checked: true }
            CheckBox {	name: "priorPosteriorPlotAdditionalTestingInfo";		label: qsTr("Testing info"); info: qsTr("Adds the Bayes factor computed with the user-defined prior; adds a probability wheel depicting the odds of the data under the null vs. alternative hypothesis (Assuming null and alternative had equal probability a priori).")	;	checked: true }
		}
		CheckBox
		{
                        name: "bfRobustnessPlot";			label: qsTr("Bayes factor robustness check"); info: qsTr("Displays the Bayes factor as a function of the width of the stretched beta prior on the correlation for each specified pair of variables. The width of the kappa prior is varied between 0 and 2.")
            CheckBox {	name: "bfRobustnessPlotAdditionalInfo";	label: qsTr("Additional info");		info: qsTr("Adds the Bayes factor computed with the user-defined prior and the maximum obtainable Bayes factor.")	;		checked: true }
		}
		CheckBox
		{
                        name: "bfSequentialPlot";			label: qsTr("Sequential analysis"); info: qsTr("Displays the development of the Bayes factor as the data come in using the user-defined prior for each specified pair of variables.")
            CheckBox {	name: "bfSequentialPlotAdditionalInfo";	label: qsTr("Additional info");	info: qsTr("Adds the Bayes factor computed with the user-defined prior; adds a probability wheel depicting the odds of the data under the null vs. alternative hypothesis; shows the decisiveness of the evidence in terms of Jeffreys' (1961) evidence categories.")	; checked: true}
		}
	}

	Section
	{
		title: qsTr("Options")

		RadioButtonGroup
		{
            name: "naAction"
			title: qsTr("Missing Values"); 
            RadioButton {	value: "pairwise";	label: qsTr("Exclude cases pairwise"); info: qsTr("Uses all complete observations for each individual pair of variables.")	; checked: true	}
            RadioButton {	value: "listwise";	label: qsTr("Exclude cases listwise"); info: qsTr("Uses only complete cases across all variables.")					}
		}

		SetSeed{}
	}
}
