// Copyright (C) 2013-2018 University of Amsterdam
//
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


Form {

	Formula
	{
		lhs: "dependent"
		rhs: [{ name: "modelTerms", extraOptions: "isNuisance" }]
	}

	VariablesForm
	{
		AvailableVariablesList	{ name: "allVariablesList" }
		AssignedVariablesList	{ name: "dependent";	title: qsTr("Dependent Variable");		suggestedColumns: ["scale"];	singleVariable: true	}
		AssignedVariablesList	{ name: "covariates";	title: qsTr("Covariates");				suggestedColumns: ["scale"];	allowedColumns: ["scale"]}
		AssignedVariablesList	{ name: "weights";		title: qsTr("WLS Weights (optional)");	suggestedColumns: ["scale"];	singleVariable: true	}
	}

	BayesFactorType {}

	Group
	{
		title: qsTr("Output")
		columns: 1

		CheckBox{ name: "posteriorSummaryTable"; label: qsTr("Posterior summary"); id: posteriorSummaryTable
			RadioButtonGroup
			{
				name: "effectsType"
				RadioButton { value: "allModels";		label: qsTr("Across all models");   checked: true	}
				RadioButton { value: "matchedModels";	label: qsTr("Across matched models")				}
			}
		}

		CheckBox
		{
			name: "posteriorSummaryPlot"
			label: qsTr("Plot of coefficients")
			id: posteriorSummaryPlot
			CheckBox { name: "posteriorSummaryPlotWithoutIntercept"; label: qsTr("Omit intercept") }

		}

		DropDown
		{
			name: "summaryType"
			enabled: posteriorSummaryTable.checked || posteriorSummaryPlot.checked
			indexDefaultValue: 3
			values: [
				{ label: qsTr("Best model"),			value: "best"		},
				{ label: qsTr("Most complex model"),	value: "complex"	},
				{ label: qsTr("Median model"),			value: "median"		},
				{ label: qsTr("Model averaged"),		value: "averaged"	}
			]
			id: summaryType
		}



		CIField
		{
			name: "posteriorSummaryPlotCiLevel"
			label: qsTr("Credible interval")
			enabled: posteriorSummaryTable.checked || posteriorSummaryPlot.checked
		}

	}

	RadioButtonGroup
	{
		name: "bayesFactorOrder"
		title: qsTr("Order")
		RadioButton { value: "bestModelTop"; label: qsTr("Compare to best model"); checked: true	}
		RadioButton { value: "nullModelTop"; label: qsTr("Compare to null model")					}
	}

	RadioButtonGroup
	{
		name: "modelsShown"
		title: qsTr("Limit No. Models Shown")
		RadioButton { value: "unlimited"; label: qsTr("No") }
		RadioButton { 
			value: "limited"
			label: qsTr("Yes, show best")
			checked: true
			childrenOnSameRow: true
			IntegerField { name: "numModelsShown"; defaultValue: 10; min: 1}
		}

	}

	Group
	{
		title: qsTr("Data")
		CheckBox { name: "descriptives"; label: qsTr("Descriptives") }
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
				source: ['covariates']
			}

			ModelTermsList { width: parent.width * 5 / 9 }

		}

	}
	
	Section
	{
		title: qsTr("Plots")

		Group
		{
			title: qsTr("Coefficients")
			CheckBox { name: "inclusionProbabilitiesPlot";	label: qsTr("Inclusion probabilities")			}
			CheckBox { name: "marginalPosteriorPlot";	label: qsTr("Marginal posterior distributions")	}
		}

		Group
		{
			title: qsTr("Models")
			CheckBox { name: "logPosteriorOddsPlot";	label: qsTr("Log posterior odds")				}
			CheckBox { name: "modelComplexityPlot";		label: qsTr("Log(P(data|M)) vs. model size")	}
			CheckBox { name: "modelProbabilitiesPlot";	label: qsTr("Model probabilities")				}
		}

		Group
		{
			title: qsTr("Residuals")
			CheckBox { name: "residualsVsFittedPlot";	label: qsTr("Residuals vs. fitted")					}
			CheckBox { name: "qqPlot";				label: qsTr("Q-Q plot of model averaged residuals")	}
		}
	}
	
	Section
	{
		title: qsTr("Advanced Options")

		Group
		{
			title: qsTr("Append columns to data")
			Layout.columnSpan: 2
			CheckBox
			{
				id:							residualsSavedToData
				name:						"residualsSavedToData"
				text:						qsTr("Residuals (%1)").arg(summaryType.currentLabel)

				ComputedColumnField
				{
					name:					"residualsSavedToDataColumn"
					text:					qsTr("Column name")
					placeholderText:		qsTr("e.g., residuals")
					fieldWidth:				120
					enabled:				residualsSavedToData.checked
				}
			}
			CheckBox
			{
				id:							residualSdsSavedToData
				name:						"residualSdsSavedToData"
				text:						qsTr("Residuals std. deviations (%1)").arg(summaryType.currentLabel)

				ComputedColumnField
				{
					name:					"residualSdsSavedToDataColumn"
					text:					qsTr("Column name")
					placeholderText:		qsTr("e.g., residual sd")
					fieldWidth:				120
					enabled:				residualSdsSavedToData.checked
				}
			}
		}


		RadioButtonGroup
		{
			name: "priorRegressionCoefficients"
			title: qsTr("Prior")

			RadioButton { value: "aic";			label: qsTr("AIC")		}
			RadioButton { value: "bic";			label: qsTr("BIC")		}
			RadioButton { value: "ebGlobal";	label: qsTr("EB-global")}
			RadioButton { value: "ebLocal";		label: qsTr("EB-local")	}
			GridLayout
			{
				rowSpacing: jaspTheme.rowGroupSpacing
				columnSpacing: 0
				Group
				{
					RadioButton { value: "gPrior";			label: qsTr("g-prior");				id: gprior			}
					RadioButton { value: "hyperG";			label: qsTr("Hyper-g");				id: hyperg			}
					RadioButton { value: "hyperGLaplace";	label: qsTr("Hyper-g-Laplace");		id: hyperglaplace	}
					RadioButton { value: "hyperGN";			label: qsTr("Hyper-g-n");			id: hypergn			}
				}
				DoubleField
				{
					name: "gPriorAlpha"
					label: qsTr("alpha")
					enabled: gprior.checked || hyperg.checked || hyperglaplace.checked || hypergn.checked
					defaultValue: 3.0
					min: 2
					max: 4
					inclusive: JASP.None
				}
				RadioButton { value: "jzs"; label: qsTr("JZS"); checked: true; id: jzs }
				DoubleField
				{
					name: "jzsRScale"
					label: qsTr("r scale")
					enabled: jzs.checked
					fieldWidth: 50
					defaultValue: 0.354
					max: 100000
					inclusive: JASP.MaxOnly
				}
			}
		}

		ColumnLayout
		{
			RadioButtonGroup
			{
				name: "modelPrior"
				title: qsTr("Model Prior")
				RadioButton
				{
					checked: true
					value: "betaBinomial"; label: qsTr("Beta binomial")
					childrenOnSameRow: true
					childrenArea.columnSpacing: 1
					DoubleField { name: "betaBinomialParamA"; label: qsTr("a"); defaultValue: 1; inclusive: JASP.MaxOnly}
					DoubleField { name: "betaBinomialParamB"; label: qsTr("b"); defaultValue: 1; inclusive: JASP.MaxOnly}
				}
				RadioButton { value: "uniform"; label: qsTr("Uniform")}
				RadioButton
				{
					value: "wilson"
					label: qsTr("Wilson")
					childrenOnSameRow: true
					childrenArea.columnSpacing: 1
					DoubleField { name: "wilsonParamLambda"; label: qsTr("λ"); defaultValue: 1; inclusive: JASP.None; min: 0}
				}
				RadioButton
				{
					value: "castillo"
					label: qsTr("Castillo")
					childrenOnSameRow: true
					childrenArea.columnSpacing: 1
					DoubleField { name: "castilloParamU"; label: qsTr("u"); defaultValue: 1; inclusive: JASP.MinMax; min: 1}
				}
				RadioButton
				{
					value: "bernoulli"; label: qsTr("Bernoulli")
					childrenOnSameRow: true
					DoubleField { name: "bernoulliParam"; label: qsTr("p"); defaultValue: 0.5; max: 1; inclusive: JASP.None; decimals: 3 }
				}
			}

			RadioButtonGroup
			{
				name: "samplingMethod"
				title: qsTr("Sampling Method")
				RadioButton
				{
					value: "bas"; label: qsTr("BAS"); checked: true
					childrenOnSameRow: true
					IntegerField { name: "numberOfModels"; label: qsTr("No. models"); defaultValue: 0; max: 100000000 }
				}
				RadioButton
				{
					value: "mcmc"; label: qsTr("MCMC")
					childrenOnSameRow: true
					IntegerField { name: "samples"; label: qsTr("No. samples"); defaultValue: 0; max: 100000000 }
				}
			}

			Group
			{
				title: qsTr("Numerical Accuracy")
				IntegerField
				{
					name: "numericalAccuracy"
					label: qsTr("No. samples for credible interval")
					defaultValue: 1000
					fieldWidth: 50
					min: 100
					max: 1000000
				}
			}

			SetSeed{}

		}
	}

}
