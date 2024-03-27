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
import "./common"		as Common

Form
{
	id: form
	property int analysis:	Common.Type.Analysis.LinearRegression
	property int framework:	Common.Type.Framework.Classical

	Formula
	{
		lhs: "dependent"
		rhs: [{ name: "modelTerms", extraOptions: "isNuisance" }]
		userMustSpecify: "covariates"
	}

	VariablesForm
	{
		AvailableVariablesList { name: "allVariablesList" }
		AssignedVariablesList { name: "dependent";	title: qsTr("Dependent Variable");	suggestedColumns: ["scale"]; singleVariable: true;		}
		DropDown
		{
			name: "method"
			label: qsTr("Method")
			values: [
				{ label: qsTr("Enter"),		value: "enter"},
				{ label: qsTr("Backward"),	value: "backward"},
				{ label: qsTr("Forward"),	value: "forward"},
				{ label: qsTr("Stepwise"),	value: "stepwise"}
			]
		}
		AssignedVariablesList { name: "covariates";	title: qsTr("Covariates");				allowedColumns: ["ordinal", "scale"]					}
		AssignedVariablesList { name: "factors";	title: qsTr("Factors");					allowedColumns: ["ordinal", "nominal", "nominalText"];	}
		AssignedVariablesList { name: "weights";	title: qsTr("WLS Weights (optional)");	allowedColumns: ["scale"]; singleVariable: true			}
	}


	Section
	{
		title: qsTr("Model")

		VariablesForm
		{
			preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
			AvailableVariablesList
			{
				name: "availableTerms"
				title: qsTr("Components")
				width: parent.width / 4
				source: ['covariates', 'factors']
			}
			ModelTermsList { width: parent.width * 5 / 9 }
		}

		CheckBox { name: "interceptTerm"; label: qsTr("Include intercept"); checked: true }
	}

	Section
	{
		title: qsTr("Statistics")

		columns: 2
		Group
		{
			title: qsTr("Coefficients")
			CheckBox
			{
				name: "coefficientEstimate"
				label: qsTr("Estimates")
				checked: true
				onClicked: { if (!checked && bootstrapping.checked) bootstrapping.click() }
				CheckBox
				{
					id: bootstrapping
					name: "coefficientBootstrap"
					label: qsTr("From")
					childrenOnSameRow: true
					IntegerField
					{
						name: "coefficientBootstrapSamples"
						defaultValue: 5000
						fieldWidth: 50
						min: 100
						afterLabel: qsTr("bootstraps")
					}
				}
			}

			CheckBox
			{
				name: "coefficientCi"; label: qsTr("Confidence intervals")
				childrenOnSameRow: true
				CIField { name: "coefficientCiLevel" }
			}
			CheckBox { name: "collinearityStatistic";		label: qsTr("Tolerance and VIF")		}
			CheckBox { name: "vovkSellke"; label: qsTr("Vovk-Sellke maximum p-ratio") }
		}
		

		Group
		{
			title: qsTr("Model Summary")
			CheckBox { name: "rSquaredChange";				label: qsTr("R squared change")				}
			CheckBox { name: "fChange";						label: qsTr("F change")				}
			CheckBox { name: "modelAICBIC";					label: qsTr("AIC and BIC")				}
			
		}

		Group
		{
			title: qsTr("Display")
			CheckBox { name: "modelFit";					label: qsTr("Model fit");  checked: true		}
			CheckBox { name: "descriptives";				label: qsTr("Descriptives")					}
			CheckBox { name: "partAndPartialCorrelation";	label: qsTr("Part and partial correlations")	}
			CheckBox { name: "covarianceMatrix"; label: qsTr("Coefficients covariance matrix") }
			CheckBox { name: "collinearityDiagnostic";		label: qsTr("Collinearity diagnostics")		}

		}

		

	Common.OutlierComponent { id: outlierComponentt}

	}

	Section
	{
		title: qsTr("Method Specification")
		columns: 1

		RadioButtonGroup
		{
			name: "steppingMethodCriteriaType"
			title: qsTr("Stepping Method Criteria")
			RadioButton
			{
				value: "pValue"; label: qsTr("Use p value"); checked: true
				columns: 2
				DoubleField { name: "steppingMethodCriteriaPEntry";		label: qsTr("Entry");	fieldWidth: 60; defaultValue: 0.05; max: 1; decimals: 3 }
				DoubleField { name: "steppingMethodCriteriaPRemoval";	label: qsTr("Removal");	fieldWidth: 60; defaultValue: 0.1; max: 1; decimals: 3	}
			}
			RadioButton
			{
				value: "fValue"; label: qsTr("Use F value")
				columns: 2
				DoubleField { name: "steppingMethodCriteriaFEntry";		label: qsTr("Entry");	fieldWidth: 60; defaultValue: 3.84; decimals: 3 }
				DoubleField { name: "steppingMethodCriteriaFRemoval";	label: qsTr("Removal");	fieldWidth: 60; defaultValue: 2.71; decimals: 3 }
			}
		}

		RadioButtonGroup
		{
			name: "naAction"
			title: qsTr("Missing Values")
			debug: true
			RadioButton { value: "listwise"; label: qsTr("Exclude cases listwise"); checked: true	}
			RadioButton { value: "pairwise"; label: qsTr("Exclude cases pairwise")					}
		}
	}

	Section
	{
		title: qsTr("Plots")

		Group
		{
			title: qsTr("Residuals Plots")
			CheckBox { name: "residualVsDependentPlot";	label: qsTr("Residuals vs. dependent")					}
            CheckBox { name: "residualVsCovariatePlot";	label: qsTr("Residuals vs. covariates")					}
			CheckBox { name: "residualVsFittedPlot";	label: qsTr("Residuals vs. predicted")					}
			CheckBox
			{
                name: "residualHistogramPlot";	label: qsTr("Residuals histogram")
                CheckBox { name: "residualHistogramStandardizedPlot";	label: qsTr("Standardized residuals"); checked: true	}
			}
			CheckBox { name: "residualQqPlot";			label: qsTr("Q-Q plot standardized residuals")			}
            CheckBox
            {
                name: "partialResidualPlot";	label: qsTr("Partial plots")
                CheckBox
                {
                    name: "partialResidualPlotCi";   label: qsTr("Confidence intervals")
                    childrenOnSameRow: true
                    CIField { name: "partialResidualPlotCiLevel"; }
                }
                CheckBox
                {
                    name: "partialResidualPlotPredictionInterval";   label: qsTr("Prediction intervals")
                    childrenOnSameRow: true
                    CIField { name: "partialResidualPlotPredictionIntervalLevel"; }
                }
            }
		}

        Group
        {
            title: qsTr("Other Plots")
            CheckBox {
                name: "marginalPlot"; label: qsTr("Marginal effects plots")
                CheckBox
                {
                    name: "marginalPlotCi"; label: qsTr("Confidence intervals")
                    childrenOnSameRow: true
                    CIField { name: "marginalPlotCiLevel"; }
                }
                CheckBox
                {
                    name: "marginalPlotPredictionInterval"; label: qsTr("Prediction intervals")
                    childrenOnSameRow: true
                    CIField { name: "marginalPlotPredictionIntervalLevel"; }
                }
            }
        }
	}
}
