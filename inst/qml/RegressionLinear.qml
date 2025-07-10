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
	info: qsTr("Linear regression allows the user to model a linear relationship between one or more explanatory variable(s) (predictors) and a continuous dependent (response) variable.\n") +
	"## " + qsTr("Assumptions") + "\n" + "- Continuous response variable.\n" + "- Linearity and additivity: The response variable is linearly related to all predictors and the effects of the predictors are additive.\n" +
	"- Independence of residuals: The residuals are uncorrelated with each other.\n" + "- Homoscedasticity: The error variance of each predictor is constant across all values of that predictor.\n" +
	"- Normality of residuals: The residuals are normally distributed with mean zero."
	id: form
	property int analysis:	Common.Type.Analysis.LinearRegression
	property int framework:	Common.Type.Framework.Classical

	VariablesForm
	{
		AvailableVariablesList { name: "allVariablesList" }
		AssignedVariablesList { name: "dependent";	title: qsTr("Dependent Variable"); info: qsTr("Dependent variable.")	; allowedColumns: ["scale"]; singleVariable: true;		}
		DropDown
		{
			name: "method"
			label: qsTr("Method")
			info: qsTr("Specify the order in which the predictors are entered into the model. A block of one or more predictors represents one step in the hierarchy. Note that the present release does not allow for more than one block.")
			values: [
				{ label: qsTr("Enter"),	info: qsTr("All predictors are entered into the model simultaneously.")	,value: "enter"},
				{ label: qsTr("Backward"), info: qsTr("All predictors are entered simultaneously, and then removed sequentially based on the criterion specified in Stepping method criteria."), value: "backward"},
				{ label: qsTr("Forward"), info: qsTr("Predictors are entered sequentially based on the criterion specified in Stepping method criteria.")	, value: "forward"},
				{ label: qsTr("Stepwise"), info: qsTr("Predictors are entered sequentially based on the criterion specified in Stepping method criteria; after each step, the least useful predictor is removed."), value: "stepwise"}
			]
		}
		AssignedVariablesList { name: "covariates";	title: qsTr("Covariates");	info: qsTr("Continuous predictor variable(s). If ordinal variables are entered it is assumed that their levels are equidistant. Hence, ordinal variables are treated as continuous predictor variables.")	;		allowedColumns: ["scale"];   minNumericLevels: 2					}
		AssignedVariablesList { name: "factors";	title: qsTr("Factors");		info: qsTr("Categorical predictor variable(s). Ordinal variables here are treated as categorical predictor variables, thus, the ordinal information is ignored.")	;		allowedColumns: ["nominal"]; minLevels: 2}
		AssignedVariablesList { name: "weights";	title: qsTr("WLS Weights (optional)"); info: qsTr("The weights used for weighted least squares regression.")	; allowedColumns: ["scale"];   singleVariable: true			}
	}

	Section
	{
		title: qsTr("Model"); info: qsTr("Components and model terms.")

		FactorsForm
		{
			id:					factors
			name:				"modelTerms"
			info: qsTr("Model terms: The independent variables in the model. By default, all the main effects of the specified independent variables are included in the model. To include interactions, click multiple variables by holding the ctrl/command button while clicking, and drag those into the Model Terms box.")
			nested:				nested.checked
			allowInteraction:	true
			initNumberFactors:	2
			baseName:			"model"
			baseTitle:			"Model"
			availableVariablesList.source: ['covariates', 'factors']
			startIndex:			0
			availableVariablesListName: "availableTerms"
			allowedColumns:		[]
		}

		CheckBox
		{
			id:			nested
			label:		"Nested"
			name:		"nested"
			checked:	true
			visible: 	false
		}

		CheckBox { name: "interceptTerm"; label: qsTr("Include intercept"); info: qsTr("Include the intercept in the regression model.") ; checked: true }
	}

	Section
	{
		title: qsTr("Statistics")

		columns: 2
		Group
		{
			title: qsTr("Model Summary")
			CheckBox { name: "rSquaredChange";				label: qsTr("R squared change")	; info: qsTr("Change in R squared between the different models, with corresponding significance test.")			}
			CheckBox { name: "fChange";						label: qsTr("F change")	; info: qsTr("Change in F-statistic between the different models, with corresponding significance test.")			}
			CheckBox { name: "modelAICBIC";					label: qsTr("AIC and BIC")	; info: qsTr("Displays Akaike Information Criterion and Bayesian Information Criterion.")			}
			CheckBox { name: "residualDurbinWatson";		label: qsTr("Durbin-Watson"); info: qsTr("Durbin-Watson statistic to test the autocorrelation of the residuals.")	}
			
		}
		
		Group
		{
			title: qsTr("Coefficients")
			CheckBox
			{
				name: "coefficientEstimate"
				label: qsTr("Estimates")
				info: qsTr("Unstandardized and standardized coefficient estimates, standard errors, t-values, and their corresponding p-values.")
				checked: true
				onClicked: { if (!checked && bootstrapping.checked) bootstrapping.click() }
				CheckBox
				{
					id: bootstrapping
					name: "coefficientBootstrap"
					label: qsTr("From")
					info: qsTr("Apply bootstrapped estimation. By default, the number of replications is set to 1000, but this can be changed to the desired number.")
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
				name: "coefficientCi"; label: qsTr("Confidence intervals"); info: qsTr("Includes confidence intervals for the estimated mean difference. By default the confidence level is set to 95%, but this can be changed to the desired percentage.")
				childrenOnSameRow: true
				CIField { name: "coefficientCiLevel" }
			}
			CheckBox { name: "collinearityStatistic";		label: qsTr("Tolerance and VIF"); info: qsTr("Displays Tolerance and Variance Inflation Factor for each predictor in the model in order to assess multicollinearity.")		}
			CheckBox { name: "vovkSellke"; label: qsTr("Vovk-Sellke maximum p-ratio"); info: qsTr("Shows the maximum ratio of the likelihood of the obtained p-value under H1 vs the likelihood of the obtained p value under H0. For example, if the two-sided p-value equals .05, the Vovk-Sellke MPR equals 2.46, indicating that this p-value is at most 2.46 times more likely to occur under H1 than under H0.") }
		}

		Group
		{
			title: qsTr("Display")
			CheckBox { name: "modelFit";					label: qsTr("Model fit"); info: qsTr("Separate ANOVA table for each model, i.e., each step in Backward, Forward, and Stepwise regression.") ; checked: true		}
			CheckBox { name: "descriptives";				label: qsTr("Descriptives")	; info: qsTr("Samples size, sample mean, sample standard deviation, and standard error of the mean.")				}
			CheckBox { name: "partAndPartialCorrelation";	label: qsTr("Part and partial correlations"); info: qsTr("Semipartial and partial correlations.")	}
			CheckBox { name: "covarianceMatrix"; label: qsTr("Coefficients covariance matrix"); info: qsTr("Displays the covariance matrix of the predictor variables, per model.") }
			CheckBox { name: "collinearityDiagnostic";		label: qsTr("Collinearity diagnostics")	; info: qsTr("Collinearity statistics, eigenvalues, condition indices, and variance proportions.")	}

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
				value: "pValue"; label: qsTr("Use p value"); info: qsTr("Use p-value as criterion for adding and removing predictors in Backward, Forward, and Stepwise regression.") ; checked: true
				columns: 2
				DoubleField { name: "steppingMethodCriteriaPEntry";		label: qsTr("Entry"); info: qsTr("Adds predictor if the p-value of the regression coefficient < x; default is x=0.05.")	;fieldWidth: 60; defaultValue: 0.05; max: 1; decimals: 3 }
				DoubleField { name: "steppingMethodCriteriaPRemoval";	label: qsTr("Removal"); info: qsTr("Removes predictor if the p-value of the regression coefficient > x; default is x=0.1.")	;fieldWidth: 60; defaultValue: 0.1; max: 1; decimals: 3	}
			}
			RadioButton
			{
				value: "fValue"; label: qsTr("Use F value"); info: qsTr("Use F-value as criterion for adding and removing predictors.")
				columns: 2
				DoubleField { name: "steppingMethodCriteriaFEntry";		label: qsTr("Entry"); info: qsTr("Adds predictor if the F-value (t^2) of the regression coefficient is > x; default is x=3.84.")	;fieldWidth: 60; defaultValue: 3.84; decimals: 3 }
				DoubleField { name: "steppingMethodCriteriaFRemoval";	label: qsTr("Removal"); info: qsTr("Removes predictor if the F-value (t^2) of the regression coefficient is < x; default is x=2.71.")	;fieldWidth: 60; defaultValue: 2.71; decimals: 3 }
			}
		}

		RadioButtonGroup
		{
			name: "naAction"
			title: qsTr("Missing Values")
			debug: true
			RadioButton { value: "listwise"; label: qsTr("Exclude cases listwise"); info: qsTr("Uses all complete observations for each individual pair of variables.") ;checked: true	}
			RadioButton { value: "pairwise"; label: qsTr("Exclude cases pairwise")	; info: qsTr("Uses only complete cases across all variables.")				}
		}
	}

	Section
	{
		title: qsTr("Plots")

		Group
		{
			title: qsTr("Residuals Plots"); info: qsTr("If the assumptions of the linear regression model are tenable, then these residuals should scatter randomly about a horizontal line. Any systematic pattern or clustering of the residuals suggests model assumption violations.")
			CheckBox { name: "residualVsDependentPlot";	label: qsTr("Residuals vs. dependent")	; info: qsTr("Scatterplot of the values of the residuals against the dependent variable.")				}
            CheckBox { name: "residualVsCovariatePlot";	label: qsTr("Residuals vs. covariates")	; info: qsTr("Scatterplot of the values of the residuals against the predictor variables.")				}
			CheckBox { name: "residualVsFittedPlot";	label: qsTr("Residuals vs. predicted")	; info: qsTr("Scatterplot of the values of the residuals against the predicted values.")				}
			CheckBox
			{
                name: "residualHistogramPlot";	label: qsTr("Residuals histogram"); info: qsTr("Displays a histogram of residual values.")
                CheckBox { name: "residualHistogramStandardizedPlot";	label: qsTr("Standardized residuals"); info: qsTr("Use standardized residuals instead.") ; checked: true	}
			}
			CheckBox { name: "residualQqPlot";			label: qsTr("Q-Q plot standardized residuals")	; info: qsTr("Checks the validity of the distributional assumption of the data set. Specifically, the plot illustrates whether the residuals are normally distributed.")		}
            CheckBox
            {
                name: "partialResidualPlot";	label: qsTr("Partial plots"); info: qsTr("These plots are scatterplots of the residuals from two regressions - regressing the dependent variable on all other predictors, and regressing a particular predictor (i.e., as dependent variable) on all other predictors-then plotting the residuals against each other.")
                CheckBox
                {
                    name: "partialResidualPlotCi";   label: qsTr("Confidence intervals"); info: qsTr("Displays the specified confidence interval around the regression line on the plot.")
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
                name: "marginalPlot"; label: qsTr("Marginal effects plots"); info: qsTr("Visualization of how a change in a predictor affects the predicted outcome, holding other variables constant.")
                CheckBox
                {
                    name: "marginalPlotCi"; label: qsTr("Confidence intervals"); info: qsTr("Displays the specified confidence interval around the regression line on the plot.")
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

	Common.ExportFreq { id: exportComponent}

}
