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

import QtQuick 2.8
import QtQuick.Layouts 1.3
import JASP.Controls 1.0


Form
{

	VariablesForm
	{
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList{  name: "allVariablesList" }
		AssignedVariablesList {  name: "variables";				title: qsTr("Variables");	 suggestedColumns: ["ordinal", "scale"] }
		AssignedVariablesList {  name: "partialOutVariables"; title: qsTr("Partial out"); suggestedColumns: ["ordinal", "scale"] }
	}

	Group
	{
		title: qsTr("Sample Correlation Coefficient")
		CheckBox { name: "pearson";			label: qsTr("Pearson's r"); checked: true	}
		CheckBox { name: "spearman";		label: qsTr("Spearman's rho")					}
		CheckBox { name: "kendallsTauB";	label: qsTr("Kendall's tau-b")			}
	}

	Group
	{
		title: qsTr("Additional Options")
		CheckBox { name: "pairwiseDisplay";		label: qsTr("Display pairwise")									}
		CheckBox { name: "significanceReport";	label: qsTr("Report significance");				checked: true	}
		CheckBox { name: "significanceFlagged";	label: qsTr("Flag significant correlations")					}
		CheckBox
		{
			name: "ci";		label: qsTr("Confidence intervals")
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
		CheckBox { name: "vovkSellke";		label: qsTr("Vovk-Sellke maximum p-ratio")			}
		CheckBox { name: "sampleSize";		label: qsTr("Sample size") }
	}

	RadioButtonGroup
	{
		title: qsTr("Alt. Hypothesis")
		name: "alternative"
		RadioButton { value: "correlated";				label: qsTr("Correlated"); checked: true	}
		RadioButton { value: "correlatedPositively";	label: qsTr("Correlated positively")		}
		RadioButton { value: "correlatedNegatively";	label: qsTr("Correlated negatively")		}
	}

	Group
	{
		title: qsTr("Plots")
		CheckBox
		{
			name: "scatterPlot";			label: qsTr("Scatter plots")
			CheckBox { name: "scatterPlotDensity";		label: qsTr("Densities for variables")	}
			CheckBox { name: "scatterPlotStatistic";	label: qsTr("Statistics")				}
            CheckBox
            {
				name: "scatterPlotCi"; label: qsTr("Confidence intervals"); childrenOnSameRow: true
				CIField { name: "scatterPlotCiLevel" }
            }
            CheckBox {
				name: "scatterPlotPredictionInterval"; label: qsTr("Prediction intervals")
                childrenOnSameRow: true
				CIField { name: "scatterPlotPredictionIntervalLevel"; }
            }
		}
		CheckBox{ name: "plotHeatmap"; label: qsTr("Heatmap") }

	}

	Section
	{
		title: qsTr("Assumption Checks")

		Group
		{
			title: qsTr("Multivariate Normality")
			CheckBox { name: "assumptionCheckMultivariateShapiro"; label: qsTr("Shapiro")			   }
			CheckBox { name: "assumptionCheckMultivariateRoyston"; label: qsTr("Royston"); debug: true  }
			CheckBox { name: "assumptionCheckMultivariateMardia" ; label: qsTr("Mardia");  debug: true  }
			CheckBox { name: "assumptionCheckMultivariateEnergy" ; label: qsTr("Energy");  debug: true  }
		}

		Group
		{
			title: qsTr("Pairwise Normality")
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
			CheckBox { name: "meansAndSd";	label: qsTr("Means and standard deviations")				}
			CheckBox { name: "crossProducts";	label: qsTr("Cross-product deviations and covariances")		}
		}

		RadioButtonGroup
		{
			name: "naAction"
			title: qsTr("Missing Values")
			RadioButton { value: "pairwise"; label: qsTr("Exclude cases pairwise"); checked: true	}
			RadioButton { value: "listwise"; label: qsTr("Exclude cases listwise")					}
		}
	}
}
