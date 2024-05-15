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
import "./common"		as Common

Form
{
	plotWidth: 480
	plotHeight: 320
	
	Formula
	{
		lhs: "dependent"
		rhs: [{ name: "modelTerms", extraOptions: "isNuisance" }]
		userMustSpecify: "covariates"
	}

	VariablesForm
	{
		AvailableVariablesList { name: "allVariablesList" }		
        AssignedVariablesList { name: "dependent";	title: qsTr("Dependent Variable");	suggestedColumns: ["ordinal", "nominal"]; singleVariable: true	}
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
		AssignedVariablesList { name: "covariates";	title: qsTr("Covariates");			suggestedColumns: ["scale"]											}
		AssignedVariablesList { name: "factors";	title: qsTr("Factors");				suggestedColumns: ["ordinal", "nominal"]	                        }
		AssignedVariablesList { name: "weights";	title: qsTr("WLS Weights (optional)"); suggestedColumns: ["scale"]; singleVariable: true; debug: true	}
	}
	
	Section
	{
		title: qsTr("Model")

		FactorsForm
		{
			id:					factors
			name:				"modelTerms"
			allowAll:			true
			nested:				nested.checked
			allowInteraction:	true
			initNumberFactors:	2
			baseName:			"model"
			baseTitle:			"Model"
			availableVariablesList.source: ['covariates', 'factors']
			startIndex:			0
			availableVariablesListName: "availableTerms"
		}

		CheckBox
		{
			id:			nested
			label:		"Nested"
			name:		"nested"
			checked:	true
			visible: 	false
		}

		CheckBox { name: "interceptTerm"; label: qsTr("Include intercept"); checked: true }
	}

	
	Section
	{
		title: qsTr("Statistics")
		
		Group
		{
			title: qsTr("Descriptives")
			CheckBox { name: "descriptives"; label: qsTr("Factor descriptives") }
		}

		Group
		{
			title: qsTr("Performance Diagnostics")
			CheckBox
			{
				name: "confusionMatrix";	label: qsTr("Confusion matrix")
			}
		}

		Group
		{
			title: qsTr("Coefficients")
			CheckBox { name: "coefficientEstimate";	label: qsTr("Estimates"); checked: true; id: coefficientEstimate
                onClicked: { if (!checked && bootstrapping.checked) bootstrapping.click() }
                CheckBox
                {
                    id: bootstrapping
                    name: "coefficientBootstrap"; label: qsTr("From")
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

				CheckBox { name: "coefficientStandardized";		label: qsTr("Standardized coefficients")	}
				CheckBox { name: "oddsRatio";		label: qsTr("Odds ratios")					}
				CheckBox
				{
								name: "coefficientCi";			label: qsTr("Confidence intervals")
					CIField {	name: "coefficientCiLevel";	label: qsTr("Interval")				}
					CheckBox {	name: "coefficientCiAsOddsRatio";			label: qsTr("Odds ratio scale")		}
				}
				CheckBox { name: "robustSe";		label: qsTr("Robust standard errors")	}
				CheckBox { name: "vovkSellke";	label: qsTr("Vovk-Sellke maximum p-ratio")	}
			}

            CheckBox { name: "multicollinearity"; label: qsTr("Multicollinearity diagnostics") }
		}        

        Group
        {
			title: qsTr("Performance Metrics")
            CheckBox { name: "accuracy";	    label: qsTr("Accuracy")			}
            CheckBox { name: "auc";			label: qsTr("AUC")					}
            CheckBox { name: "sensitivity";		label: qsTr("Sensitivity / Recall")	}
            CheckBox { name: "specificity";		label: qsTr("Specificity")			}
			CheckBox { name: "precision";		label: qsTr("Precision")			}
			CheckBox { name: "fMeasure";		label: qsTr("F-measure")			}
            CheckBox { name: "brierScore";	label: qsTr("Brier score")			}
			CheckBox { name: "hMeasure";		label: qsTr("H-measure")			}
        }
	
		Common.OutlierComponent { id: outlierComponent}


	}
	
	Section
	{
		title: qsTr("Plots")
		
		Group
		{
			title: qsTr("Inferential Plots")
			CheckBox
			{
							name: "conditionalEstimatePlot";	label: qsTr("Conditional estimates plots")
				CIField {	name: "conditionalEstimatePlotCi";	label: qsTr("Confidence interval")					}
				CheckBox {	name: "conditionalEstimatePlotPoints";			label: qsTr("Show data points")						}
			}
		}

		Group
		{
			title: qsTr("Diagnostic Plots")
			CheckBox { name: "residualVsFittedPlot";		label: qsTr("Predicted - residual plot")		}
			CheckBox { name: "residualVsPredictorPlot";		label: qsTr("Predictor - residual plots")		}
			CheckBox { name: "squaredPearsonResidualVsFittedPlot";	label: qsTr("Squared Pearson residuals plot")	}
			CheckBox 
			{ 
				name: "independentVsPredictedPlot"
				label: qsTr("Independent - logit(predicted) plot")	
				CheckBox { name: "independentVsPredictedPlotIncludeInteractions";	label: qsTr("Include interactions"); checked: true}
			}

		}

		RadioButtonGroup
		{
			name: "residualType"
			title: qsTr("Residual Type")
			RadioButton { value: "deviance";	label: qsTr("Deviance");	checked: true   }
			RadioButton { value: "pearson";		label: qsTr("Pearson")					}
		}

        Group
        {
            title: qsTr("Performance Plots")
            CheckBox
            {
                              name: "rocPlot";               label: qsTr("ROC plot")
                DoubleField { name: "rocPlotCutoffStep";              label: qsTr("Cutoff step"); defaultValue: 0.2; min: 0.05; max: 0.5; decimals: 3     }
                CheckBox    { name: "rocPlotCutoffLabel";   label: qsTr("Add cutoff labels")                                                    }
            }
            CheckBox
            {
                              name: "precisionRecallPlot";                label: qsTr("PR plot")
                DoubleField { name: "precisionRecallPlotCutoffStep";               label: qsTr("Cutoff step"); defaultValue: 0.2; min: 0.05; max: 0.5; decimals: 3     }
                CheckBox    { name: "precisionRecallPlotCutoffLabel";    label: qsTr("Add cutoff labels")                                                    }
            }
        }
	}
}
