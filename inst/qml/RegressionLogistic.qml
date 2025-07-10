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
	info: qsTr("Logistic regression allows the user to model a linear relationship between one or more explanatory variable/s and a categorical dependent variable.\n") +
	"## " + qsTr("Assumptions") + "\n" + "- The dependent variables are categorical.\n" + "- The dependent variable is linearly related to all predictors and the effects of the predictors are additive.\n" +
	"- The assumption of homoscedasticity is met. Homoscedasticity entails that the error variance of each predictor is constant across all values of that predictor.\n" +
	"- The residuals are uncorrelated with each other.\n" + "- The residuals are normally distributed with a mean of zero.\n" + "- The covariate and the experiment effect are independent."
	plotWidth: 480
	plotHeight: 320
	
	VariablesForm
	{
		AvailableVariablesList { name: "allVariablesList" }		
        AssignedVariablesList { name: "dependent";	title: qsTr("Dependent Variable"); info: qsTr("The variable of interest. This is also called the outcome variable. In case of multiple dependent variables, specify the order in which the predictors are entered into the model i.e., hierarchical regression analysis. A block of one or more predictors represents one step in the hierarchy.")	; allowedColumns: ["nominal"]; singleVariable: true	}
		DropDown
		{
			name: "method"
			label: qsTr("Method")
			values: [
				{ label: qsTr("Enter"),	info: qsTr("All predictors are entered into the model simultaneously. Forced entry.")	, value: "enter"},
				{ label: qsTr("Backward"), info: qsTr("All predictors are entered simultaneously, and then removed sequentially based on the criterion specified in stepping method criteria.")	, value: "backward"},
				{ label: qsTr("Forward"), info: qsTr("Predictors are entered sequentially based on the criterion specified in stepping method criteria."), value: "forward"},
				{ label: qsTr("Stepwise"), info: qsTr("Predictors are entered sequentially based on the criterion specified in stepping method criteria; after each step, the least useful predictor is removed.")	, value: "stepwise"}
			]
		}
		AssignedVariablesList { name: "covariates";	title: qsTr("Covariates"); info: qsTr("In this box the variable that is the covariate can be selected. Covariates are continuous variables that influence the dependent variable but are not part of the experimental manipulation.")	;		allowedColumns: ["scale"]; 		minNumericLevels: 2			}
		AssignedVariablesList { name: "factors";	title: qsTr("Factors");	info: qsTr("The variables that are manipulated/define the different groups. These are also called the independent variables.")	;		allowedColumns: ["nominal"]; 	minLevels: 2                }
		AssignedVariablesList { name: "weights";	title: qsTr("WLS Weights (optional)"); info: qsTr("The weights used for weighted least squares regression.") ;allowedColumns: ["scale"]; 	singleVariable: true; debug: true	}
	}
	
	Section
	{
		title: qsTr("Model")

		FactorsForm
		{
			id:					factors
			name:				"modelTerms"
			info: qsTr("The independent variables and covariates included in the model. By default, all the main effects and interaction effects of the specified independent variables, and the covariates are included in the model.")
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

		CheckBox { name: "interceptTerm"; label: qsTr("Include intercept"); info: qsTr("Ticking this box will add a coefficient estimate of the intercept as well. This corresponds to the first level for the independent variable.") ; checked: true }
	}

	
	Section
	{
		title: qsTr("Statistics")
		
		Group
		{
			title: qsTr("Descriptives")
			CheckBox { name: "descriptives"; label: qsTr("Factor descriptives"); info: qsTr("The levels of the dependent variable/s and the number of observations per level.") }
		}

		Group
		{
			title: qsTr("Performance Diagnostics")
			CheckBox
			{
				name: "confusionMatrix";	label: qsTr("Confusion matrix"); info: qsTr("The confusion matrix indicates how well the model predicts the outcomes. The table is showing actual versus predicted outcomes and can be used to determine the accuracy of the model.")
			}
		}

		Group
		{
			title: qsTr("Coefficients")
			CheckBox { name: "coefficientEstimate";	label: qsTr("Estimates"); info: qsTr("Coefficient estimates, standard errors, z-values, and their corresponding p-values.") ; checked: true; id: coefficientEstimate
                onClicked: { if (!checked && bootstrapping.checked) bootstrapping.click() }
                CheckBox
                {
                    id: bootstrapping
                    name: "coefficientBootstrap"; label: qsTr("From"); info: qsTr("By selecting this option, bootstrapped estimation is applied. By default, the number of replications is set to 5000. This can be changed into the desired number.")
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

				CheckBox { name: "coefficientStandardized";		label: qsTr("Standardized coefficients"); info: qsTr("Standardized estimates represent estimates where the predictors are standardized (X-standardization).")	}
				CheckBox { name: "oddsRatio";		label: qsTr("Odds ratios"); info: qsTr("Odds ratio is an indicator of the change in odds resulting from a unit change in the predictor.")	;	checked: true			}
				CheckBox
				{
								name: "coefficientCi";			label: qsTr("Confidence intervals"); info: qsTr("Coverage of the confidence intervals in percentages. The default value is 95%, which can be changed by the user.")
					CIField {	name: "coefficientCiLevel";	label: qsTr("Interval")				}
					CheckBox {	name: "coefficientCiAsOddsRatio";			label: qsTr("Odds ratio scale"); checked:true		}
				}
				CheckBox { name: "robustSe";		label: qsTr("Robust standard errors"); info: qsTr("This option controls for errors that are not independent and identically distributed. The use of robust standard errors will not change the coefficient estimates. If this option is not selected the normal standard error will be computed.")	}
				CheckBox { name: "vovkSellke";	label: qsTr("Vovk-Sellke maximum p-ratio"); info: qsTr("Shows the maximum ratio of the likelihood of the obtained p value under H1 vs the likelihood of the obtained p value under H0. For example, if the two-sided p-value equals .05, the Vovk-Sellke MPR equals 2.46, indicating that this p-value is at most 2.46 times more likely to occur under H1 than under H0.")	}
			}

            CheckBox { name: "multicollinearity"; label: qsTr("Multicollinearity diagnostics"); info: qsTr("Displays Tolerance and Variance Inflation Factor for each predictor in the model to assess multicollinearity.") }
		}        

        Group
        {
			title: qsTr("Performance Metrics")
            CheckBox { name: "accuracy";	    label: qsTr("Accuracy")	; info: qsTr("How often the model's prediction match the actual outcomes.")		}
            CheckBox { name: "auc";			label: qsTr("AUC")		; info: qsTr("Area Under the Curve.")			}
            CheckBox { name: "sensitivity";		label: qsTr("Sensitivity / Recall"); info: qsTr("Sensitivity describes the proportion of true positives.")	}
            CheckBox { name: "specificity";		label: qsTr("Specificity"); info: qsTr("Specificity describes the proportion of true negatives.")			}
			CheckBox { name: "precision";		label: qsTr("Precision")	; info: qsTr("Precision describes the proportion of true positives to all positives. Also called the positive predictive value.")		}
			CheckBox { name: "fMeasure";		label: qsTr("F-measure"); info: qsTr("The F-measure is based on the amount of systematic variance divided by the amount of unsystematic variance i.e., mean squares for the model / the residual mean squares.")			}
            CheckBox { name: "brierScore";	label: qsTr("Brier score")	; info: qsTr("Another measure of the accuracy of predictions. Based on the mean squared difference between predicted probabilities and the binary outcome. Lower scores mean more accuracy.")		}
			CheckBox { name: "hMeasure";		label: qsTr("H-measure"); info: qsTr("A more theoretically robust and advanced alternative to the AUC.")			}
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
							name: "conditionalEstimatePlot";	label: qsTr("Conditional estimates plots"); info: qsTr("The plots are conditional in the sense that they display the probability of the categorical dependent variable for all levels of the predictor variables given the reference level of all other factors.")
				CIField {	name: "conditionalEstimatePlotCi";	label: qsTr("Confidence interval")	; info: qsTr("Displays the specified confidence interval around the regression line on the plot.")				}
				CheckBox {	name: "conditionalEstimatePlotPoints";			label: qsTr("Show data points"); info: qsTr("Displays individual data points on the plot.")						}
			}
		}

		Group
		{
			title: qsTr("Diagnostic Plots"); info: qsTr("If the assumptions of the linear regression model hold, then these residuals should scatter randomly about a horizontal line. Any systematic pattern or clustering of the residuals suggests model assumption violations.")
			CheckBox { name: "residualVsFittedPlot";		label: qsTr("Predicted - residual plot")	; info: qsTr("Scatterplot of the values of the residuals against the predicted values.")	}
			CheckBox { name: "residualVsPredictorPlot";		label: qsTr("Predictor - residual plots"); info: qsTr("Scatterplot for every independent variable and covariate of the residuals and the levels of the variable of interest.")		}
			CheckBox { name: "squaredPearsonResidualVsFittedPlot";	label: qsTr("Squared Pearson residuals plot"); info: qsTr("With the Squared Pearson residuals plot one can check for overdispersion of the model. Overdispersion indicates that the actual data show greater variability than the model has predicted.")	}
			CheckBox 
			{ 
				name: "independentVsPredictedPlot"
				label: qsTr("Independent - predicted plot")	
				info: qsTr("Plots the model predictions against each independent variable.")
				CheckBox { name: "independentVsPredictedPlotIncludeInteractions";	label: qsTr("Include interactions"); info: qsTr("Adds every two-way interaction.") ;checked: true}
				CheckBox { name: "independentVsPredictedPlotUseLogit";	label: qsTr("Use logit scale"); info: qsTr("Plots predicted probabilities on the logit scale, to ensure a linear relation.") ;checked: true}

			}

		}

		RadioButtonGroup
		{
			name: "residualType"
			title: qsTr("Residual Type")
			RadioButton { value: "deviance";	label: qsTr("Deviance"); info: qsTr("The standardized deviance residuals.") ;	checked: true   }
			RadioButton { value: "pearson";		label: qsTr("Pearson")	; info: qsTr("The standardized Pearson residuals.")				}
		}

        Group
        {
            title: qsTr("Performance Plots")
            CheckBox
            {
                              name: "rocPlot";               label: qsTr("ROC plot"); info: qsTr("Plots performance of the model by plotting the true positive rate to the false positive rate. The cutoff step determines the threshold at which an observation is classified as positive or a negative.")
                DoubleField { name: "rocPlotCutoffStep";              label: qsTr("Cutoff step"); defaultValue: 0.2; min: 0.001; max: 0.5; decimals: 3     }
                CheckBox    { name: "rocPlotCutoffLabel";   label: qsTr("Add cutoff labels") ; info: qsTr("By clicking this box, the specified cutoff step is displayed on the performance plot.")                                                  }
            }
            CheckBox
            {
                              name: "precisionRecallPlot";                label: qsTr("PR plot"); info: qsTr("Displays the Positive predicitve value on the y axis vs the true positive rate/sensitivity on the x axis. Useful for imbalanced datasets. The cutoff step determines the threshold at which an observation is classified as positive or negative. ")
                DoubleField { name: "precisionRecallPlotCutoffStep";               label: qsTr("Cutoff step"); defaultValue: 0.2; min: 0.05; max: 0.5; decimals: 3     }
                CheckBox    { name: "precisionRecallPlotCutoffLabel";    label: qsTr("Add cutoff labels") ;      info: qsTr("By clicking this box, the specified cutoff step is displayed on the precision recall plot.")                                              }
            }
        }
	}

	Common.ExportFreq { id: exportComponent}

}
