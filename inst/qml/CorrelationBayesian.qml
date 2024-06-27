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

	VariablesForm
	{
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList	{	name: "allVariablesList" }
		AssignedVariablesList	{	name: "variables"; allowedColumns: ["scale"] }
	}

	Group
	{
		title: qsTr("Population Correlation Coefficient")
		CheckBox {	name: "pearson";		label: qsTr("Pearson's rho");	checked: true	}
		CheckBox {	name: "spearman";		label: qsTr("Spearman's rho");	debug: true		}
		CheckBox {	name: "kendall";		label: qsTr("Kendall's tau-b")					}
	}

	Group
	{
		title: qsTr("Additional Options")
        CheckBox {	name: "pairwiseDisplay";                label: qsTr("Display pairwise table")					}
        CheckBox {	name: "bayesFactorReport";              label: qsTr("Report Bayes factors"); checked: true		}
        CheckBox {	name: "supportCorrelationFlagged";		label: qsTr("Flag supported correlations")				}
        CheckBox {	name: "sampleSize";                     label: qsTr("Sample size")								}
        CheckBox {	name: "posteriorMedian";                label: qsTr("Posterior median"); debug: true			}
		CheckBox
		{
			name: "ci"; label: qsTr("Credible intervals")
            CIField { name: "ciLevel";	label: qsTr("Interval") }
		}
	}

	RadioButtonGroup
	{
		id:		alternative
		name:	"alternative"
		title:	qsTr("Alt. Hypothesis")
        RadioButton {	value: "twoSided";		label: qsTr("Correlated"); checked: true	}
		RadioButton {	value: "greater";		label: qsTr("Correlated positively")		}
		RadioButton {	value: "less";			label: qsTr("Correlated negatively")		}
	}

	Group
	{
		title: qsTr("Plots")
		CheckBox
		{
                        name: "matrixPlot";				label: qsTr("Correlation matrix")
            CheckBox {	name: "matrixPlotDensity";      label: qsTr("Densities for variables")	}
            CheckBox {	name: "matrixPlotPosterior";	label: qsTr("Posteriors under H\u2081")	}
		}
	}

	BayesFactorType { correlated: alternative.value }

	Group
	{
		title: qsTr("Prior")
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
			RadioButton {	value: "pearson";		label: qsTr("Pearson's rho");	checked:	true	}
			RadioButton {	value: "spearman";		label: qsTr("Spearman's rho");	debug:		true	}
			RadioButton {	value: "kendall";		label: qsTr("Kendall's tau")						}
		}

		CheckBox
		{
                        name: "scatterPlot";		label: qsTr("Scatterplot");			checked: true
            CheckBox {	name: "scatterPlotAdditionalInfo";	label: qsTr("Robustness check");	debug: true }

		}
		CheckBox
		{
                        name: "priorPosteriorPlot";						label: qsTr("Prior and posterior")
            CheckBox {	name: "priorPosteriorPlotAdditionalEstimationInfo";	label: qsTr("Estimation info");		checked: true }
            CheckBox {	name: "priorPosteriorPlotAdditionalTestingInfo";		label: qsTr("Testing info");		checked: true }
		}
		CheckBox
		{
                        name: "bfRobustnessPlot";			label: qsTr("Bayes factor robustness check")
            CheckBox {	name: "bfRobustnessPlotAdditionalInfo";	label: qsTr("Additional info");					checked: true }
		}
		CheckBox
		{
                        name: "bfSequentialPlot";			label: qsTr("Sequential analysis")
            CheckBox {	name: "bfSequentialPlotAdditionalInfo";	label: qsTr("Additional info");		checked: true}
		}
	}

	Section
	{
		title: qsTr("Options")

		RadioButtonGroup
		{
            name: "naAction"
			title: qsTr("Missing Values")
            RadioButton {	value: "pairwise";	label: qsTr("Exclude cases pairwise");	checked: true	}
            RadioButton {	value: "listwise";	label: qsTr("Exclude cases listwise")					}
		}

		SetSeed{}
	}
}
