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
import JASP
import JASP.Controls

Form
{	info: qsTr("The Correlation analysis allows estimation of the population correlation, as well as testing the null hypothesis that the population correlation between pairs of variables equals 0. All possible pairs of the specified variables are analyzed.\n") +
	"## " + qsTr("Assumptions (Pearson's rho)") + "\n" + "- The variables are both continuous. \n" + "- The data are a random sample from the population. \n" + "- The pairs of variables follow a bivariate normal distribution in the population. \n" + "- The relationship between the variables is linear. \n" + "## " + qsTr("Assumptions (Spearman's rho and Kendall's tau).") +
    "\n" + "- Ordinal or continuous variables.\n" + "- The data are a random sample from the population.\n" + "- The relationship between the pairs of variables is monotonic."
   
    VariablesForm
	{
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		infoLabel: qsTr("Input")
		AvailableVariablesList{  name: "allVariablesList" }
		AssignedVariablesList {  name: "variables";				title: qsTr("Variables"); info: qsTr("Variables for which to compute the correlation coefficient.") ; allowedColumns: ["scale"]; minNumericLevels: 2 }
		AssignedVariablesList {  name: "partialOutVariables"; title: qsTr("Partial out"); allowedColumns: ["scale"]; info: qsTr("Variables to partial out in order to compute partial correlations.") ; minNumericLevels: 2 }
	}

	Group
	{
		title: qsTr("Sample Correlation Coefficient")
		CheckBox { name: "pearson";			label: qsTr("Pearson's r"); info: qsTr("Pearson's product moment correlation coefficient."); checked: true	}
		CheckBox { name: "spearman";		label: qsTr("Spearman's rho"); info: qsTr("Spearman's rank-order correlation coefficient to quantify the monotonic association between two variables by ranking the observations. Use when the data are not normally distributed.")				}
		CheckBox { name: "kendallsTauB";	label: qsTr("Kendall's tau-b"); info: qsTr("Kendall's tau-b rank-order correlation coefficient to quantify the monotonic association between two variables by comparing concordant and non-concordant pairs. Use when the data are not normally distributed.")			}
	}

	Group
	{
		title: qsTr("Additional Options")
		CheckBox { name: "pairwiseDisplay";		label: qsTr("Display pairwise");   checked: true; 	info: qsTr("Displays a table where one row corresponds to one pair of the specified variables, and the scatter plots are shown individually for each pair. If unticked, the results are presented in matrix format, with variable names in the columns and rows.")}
		CheckBox { name: "significanceReport";	label: qsTr("Report significance"); checked: true;  info: qsTr("Displays the p-value corresponding to the observed correlation.")}
		CheckBox { name: "significanceFlagged";	label: qsTr("Flag significant correlations"); 		info: qsTr("Marks statistically significant correlations.")					}
		CheckBox
		{
			name: "ci";		label: qsTr("Confidence intervals"); info: qsTr("Confidence intervals for the population correlation (available only for the Pearson correlation). The default is set at 95%, but this percentage can be adjusted. Additionally, you can set up bootstrap estimation, using 1000 replications by default but it can be modified to your desired number.")
			CIField { name: "ciLevel"; label: qsTr("Interval") }
			CheckBox
			{
				name: "ciBootstrap"
				label: qsTr("From")
				childrenOnSameRow: true
				IntegerField
				{
					name: "ciBootstrapSamples"
					defaultValue: 1000
					fieldWidth: 50
					min: 100
					afterLabel: qsTr("bootstraps")
				}
			}
		}

		CheckBox { name: "vovkSellke";		label: qsTr("Vovk-Sellke maximum p-ratio"); info: qsTr("Shows the maximum ratio of the likelihood of the obtained p value under H1 vs the likelihood of the obtained p value under H0. For example, if the two-sided p-value equals .05, the Vovk-Sellke MPR equals 2.46, indicating that this p-value is at most 2.46 times more likely to occur under H1 than under H0.")			}
		CheckBox { name: "effectSize";		label: qsTr("Effect size (Fisher's z)")	; info: qsTr("The Fisher transformed effect size with standard error.")	}
		CheckBox { name: "sampleSize";		label: qsTr("Sample size"); info: qsTr("The number of complete observations for a given pair of variables.") }
		CheckBox { name: "covariance";		label: qsTr("Covariance"); info: qsTr("The covariance between each pair of variables.")				}


	}

	RadioButtonGroup
	{
		title: qsTr("Alt. Hypothesis")
		name: "alternative"
		RadioButton { value: "twoSided";	label: qsTr("Correlated"); info: qsTr("Two-sided alternative hypothesis that the population correlation does not equal 0.") ; checked: true	}
		RadioButton { value: "greater";		label: qsTr("Correlated positively"); info: qsTr("One-sided alternative hypothesis that the population correlation is greater than 0.")		}
		RadioButton { value: "less";		label: qsTr("Correlated negatively"); info: qsTr("One-sided alternative hypothesis that the population correlation is less than 0.")		}
	}

	Group
	{
		title: qsTr("Plots")
		CheckBox

			name: "scatterPlot";			label: qsTr("Scatter plots"); info: qsTr("Display a scatter plot for each pairwise combination of the selected variables. In a matrix format, these are placed above the diagonal.")
			CheckBox { name: "scatterPlotDensity";		label: qsTr("Densities for variables"); info: qsTr("Display histogram and the corresponding density plot for each variable. In a matrix format, these are placed on the diagonal.")	}
			CheckBox { name: "scatterPlotStatistic";	label: qsTr("Statistics")	; info: qsTr("Display the correlation coefficient(s) in the plot. This option also adds the x% confidence interval(s) as specified in the Confidence Intervals option.")			}
            CheckBox
            {
				name: "scatterPlotCi"; label: qsTr("Confidence intervals"); info: qsTr("Displays the specified confidence interval around the regression line on the plot.") ;childrenOnSameRow: true
				CIField { name: "scatterPlotCiLevel" }
            }
            CheckBox {
				name: "scatterPlotPredictionInterval"; label: qsTr("Prediction intervals")
                childrenOnSameRow: true
				CIField { name: "scatterPlotPredictionIntervalLevel"; }
            }
		}
		CheckBox{ name: "heatmapPlot"; label: qsTr("Heatmap"); info: qsTr("Displays a correlation heatmap for Pearson, Spearman, and Kendall's tau B coefficients separately.") }

	}

	Section
	{
		title: qsTr("Assumption Checks")

		Group
		{
			title: qsTr("Multivariate Normality"); info: qsTr("Shapiro: Generalized Shapiro-Wilk test for multivariate normality by Villasenor-Alva and Gonzalez-Estrada (2009), using the mvShapiroTest package.")
			CheckBox { name: "assumptionCheckMultivariateShapiro"; label: qsTr("Shapiro")			   }
			CheckBox { name: "assumptionCheckMultivariateRoyston"; label: qsTr("Royston"); debug: true  }
			CheckBox { name: "assumptionCheckMultivariateMardia" ; label: qsTr("Mardia");  debug: true  }
			CheckBox { name: "assumptionCheckMultivariateEnergy" ; label: qsTr("Energy");  debug: true  }
		}

		Group
		{
			title: qsTr("Pairwise Normality"); info: qsTr("Shapiro: For each possible combination of the selected variables, computes the Shapiro-Wilk statistic to test the null hypothesis that the variable pair has a bivariate normal distribution.")
			CheckBox { name: "assumptionCheckPairwiseShapiro"; label: qsTr("Shapiro")			   }
			CheckBox { name: "assumptionCheckPairwiseRoyston"; label: qsTr("Royston"); debug: true  }
			CheckBox { name: "assumptionCheckPairwiseMardia" ; label: qsTr("Mardia");  debug: true  }
			CheckBox { name: "assumptionCheckPairwiseEnergy" ; label: qsTr("Energy");  debug: true  }
		}
	}

	Section
	{
		title: qsTr("Options")

		Group
		{
			title: qsTr("Statistics")
			debug: true
			CheckBox { name: "meansAndSd";	label: qsTr("Means and standard deviations.")				}
			CheckBox { name: "crossProducts";	label: qsTr("Cross-product deviations and covariances.")		}
		}

		RadioButtonGroup
		{
			name: "naAction"
			title: qsTr("Missing Values")
			RadioButton { value: "pairwise"; label: qsTr("Exclude cases pairwise"); info: qsTr("Uses all complete observations for each individual pair of variables."); checked: true	}
			RadioButton { value: "listwise"; label: qsTr("Exclude cases listwise"); info: qsTr("Uses only complete cases across all variables.")				}
		}
	}
}
