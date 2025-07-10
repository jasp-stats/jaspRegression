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
import "./common" as Common

Form {
	info: qsTr("The Bayesian Linear Regression allows you to model a linear relationship between one or more explanatory variable/s and a continuous dependent variable.\n") +
	"## " +qsTr("Assumptions") + "\n" + "- Continuous response variable.\n" + "- Linearity and additivity: The response variable is linearly related to all predictors and the effects of the predictors are additive.\n" +
	"- Independence of errors: The errors are uncorrelated with each other.\n" + "- Homoscedasticity: The error variance of each predictor is constant across all values of that predictor.\n" +
	"- Normality of errors: The errors are normally distributed with mean zero."
	Formula
	{
		lhs: "dependent"
		rhs: [{ name: "modelTerms", extraOptions: "isNuisance" }]
	}

	VariablesForm
	{
		AvailableVariablesList	{ name: "allVariablesList" }
		AssignedVariablesList	{ name: "dependent";	title: qsTr("Dependent Variable"); info: qsTr("Drag the relevant dependent variable(s) to this box.")	;	allowedColumns: ["scale"];	singleVariable: true	}
		AssignedVariablesList	{ name: "covariates";	title: qsTr("Covariates"); info: qsTr("Continuous predictor variable(s). If ordinal variables are entered it is assumed that their levels are equidistant. Hence, ordinal variables are treated as continuous predictor variables.")	;			allowedColumns: ["scale"];  minNumericLevels: 2}
		AssignedVariablesList	{ name: "weights";		title: qsTr("WLS Weights (optional)"); info: qsTr("The weights used for weighted least square regression.")	;allowedColumns: ["scale"];	singleVariable: true	}
	}

	BayesFactorType {}

	Group
	{
		title: qsTr("Output")
		columns: 1

		CheckBox{ name: "posteriorSummaryTable"; label: qsTr("Posterior summary"); info: qsTr("Output table containing the Marginal Posterior Summaries of Coefficients.") ; id: posteriorSummaryTable
			RadioButtonGroup
			{
				name: "effectsType"
				RadioButton { value: "allModels";		label: qsTr("Across all models");  info: qsTr("Compares across all models.") ;checked: true	}
				RadioButton { value: "matchedModels";	label: qsTr("Across matched models"); info: qsTr("Compares across matched models.")				}
			}
		}

		CheckBox
		{
			name: "posteriorSummaryPlot"
			label: qsTr("Plot of coefficients")
			info: qsTr("Displays plot of the most likely values of the effect size for each predictor with their corresponding credible interval.")
			id: posteriorSummaryPlot
			CheckBox { name: "posteriorSummaryPlotWithoutIntercept"; label: qsTr("Omit intercept"); info: qsTr("Omits the intercept in the plot display.") }

		}

		DropDown
		{
			name: "summaryType"
			enabled: posteriorSummaryTable.checked || posteriorSummaryPlot.checked
			indexDefaultValue: 3
			values: [
				{ label: qsTr("Best model"), info: qsTr("Displays the best model, meaning the most likely model given the data.")	,		value: "best"		},
				{ label: qsTr("Most complex model"), info: qsTr("Displays the most complex model, meaning the model which includes all the predictors.")	,value: "complex"	},
				{ label: qsTr("Median model"), info: qsTr("Displays the median model, meaning the smallest model in which all the predictors are more likely to be part of the true model than not given the data. i.e., has a posterior inclusion probability equal or greater than 0.5.")	,		value: "median"		},
				{ label: qsTr("Model averaged"), info: qsTr("Displays the model in which the predictor coefficients are averaged out across all models.")	,	value: "averaged"	}
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
		title: qsTr("Order"); info: qsTr("Compares each model against the model selected here.")
		RadioButton { value: "bestModelTop"; label: qsTr("Compare to best model"); info: qsTr("All models are compared to the best model.") ;checked: true	}
		RadioButton { value: "nullModelTop"; label: qsTr("Compare to null model"); info: qsTr("All models are compared to the null model.")					}
	}

	RadioButtonGroup
	{
		name: "modelsShown"
		title: qsTr("Limit No. Models Shown"); info: qsTr("By default, the output shows all the models computed. There is an option to show the best n models.")
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
		CheckBox { name: "descriptives"; label: qsTr("Descriptives"); info: qsTr("Output table containing the mean, standard deviation, and sample size of the variables selected.") }
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
				info: qsTr("All the independent variables that can be included in the model.")
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
			CheckBox { name: "inclusionProbabilitiesPlot";	label: qsTr("Inclusion probabilities"); info: qsTr("Shows a histogram of the posterior inclusion probabilities. The dotted line displays the prior inclusion probabilities.")			}
			CheckBox { name: "marginalPosteriorPlot";	label: qsTr("Marginal posterior distributions"); info: qsTr("Displays a plot of the marginal posterior distribution for each predictor.")	}
		}

		Group
		{
			title: qsTr("Models")
			CheckBox { name: "logPosteriorOddsPlot";	label: qsTr("Log posterior odds"); info: qsTr("Shows a heatmap of the log posterior odds against the model rank.")				}
			CheckBox { name: "modelComplexityPlot";		label: qsTr("Log(P(data|M)) vs. model size"); info: qsTr("Shows the relation between model fit and complexity.")	}
			CheckBox { name: "modelProbabilitiesPlot";	label: qsTr("Model probabilities"); info: qsTr("Displays the cumulative distribution function of the model search.")				}
		}

		Group
		{
			title: qsTr("Residuals")
			CheckBox { name: "residualsVsFittedPlot";	label: qsTr("Residuals vs. fitted")	; info: qsTr("Plots the residuals of the model averaged predictions against the residuals.")				}
			CheckBox { name: "qqPlot";				label: qsTr("Q-Q plot of model averaged residuals"); info: qsTr("Displays a Q-Q plot of the model averaged predictions against the residuals.")	}
		}
	}
	
	Section
	{
		title: qsTr("Advanced Options")

		RadioButtonGroup
		{
			name: "priorRegressionCoefficients"
			title: qsTr("Prior"); info: qsTr("Prior distribution for regression coefficients. Several options are available:")

			RadioButton { value: "aic";			label: qsTr("AIC"); info: qsTr("Compare models using the Akaike Information Criterion.")		}
			RadioButton { value: "bic";			label: qsTr("BIC"); info: qsTr("Compare models using the Bayesian Information Criterion.")		}
			RadioButton { value: "ebGlobal";	label: qsTr("EB-global"); info: qsTr("Global Empirical Bayes estimates of g in Zellner-Siow g-prior and model probabilities. Uses an EM algorithm to find a common or global estimate of g, averaged over all models. When it is not possible to enumerate all models, the EM algorithm uses only the models sampled under EB-local.")}
			RadioButton { value: "ebLocal";		label: qsTr("EB-local"); info: qsTr("Uses the MLE of g from the marginal likelihood within each model.")	}
			GridLayout
			{
				rowSpacing: jaspTheme.rowGroupSpacing
				columnSpacing: 0
				Group
				{
					RadioButton { value: "gPrior";			label: qsTr("g-prior"); info: qsTr("Zellner's g-prior.")	;			id: gprior			}
					RadioButton { value: "hyperG";			label: qsTr("Hyper-g"); info: qsTr("A mixture of g-priors where the prior on g/(1+g) is a Beta(1, alpha/2). This uses the Cephes library for evaluation of the marginal likelihoods and may be numerically unstable for large n or R2 close to 1. Default choice of alpha is 3.")	;			id: hyperg			}
					RadioButton { value: "hyperGLaplace";	label: qsTr("Hyper-g-Laplace"); info: qsTr("Same as Hyper-g but uses a Laplace approximation to integrate over the prior on g.")	;	id: hyperglaplace	}
					RadioButton { value: "hyperGN";			label: qsTr("Hyper-g-n"); info: qsTr("A mixture of g-priors where u = g/n and u Beta(1, alpha/2) to provide consistency when the null model is true.")	;		id: hypergn			}
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
				RadioButton { value: "jzs"; label: qsTr("JZS"); info: qsTr("Jeffreys-Zellner-Siow prior which uses the Jeffreys prior on sigma and the Zellner-Siow Cauchy prior on the coefficients. The optional parameter can be used to control the squared scale of the prior.") ; checked: true; id: jzs }
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
				info: qsTr("Prior distribution on the models.")
				RadioButton
				{
					checked: true
					value: "betaBinomial"; label: qsTr("Beta binomial"); info: qsTr("Default Beta(a = 1, b = 1).")
					childrenOnSameRow: true
					childrenArea.columnSpacing: 1
					DoubleField { name: "betaBinomialParamA"; label: qsTr("a"); defaultValue: 1; inclusive: JASP.MaxOnly}
					DoubleField { name: "betaBinomialParamB"; label: qsTr("b"); defaultValue: 1; inclusive: JASP.MaxOnly}
				}
				RadioButton { value: "uniform"; label: qsTr("Uniform"); info: qsTr("Uniform prior distribution.")}
				RadioButton
				{
					value: "wilson"
					label: qsTr("Wilson")
					info: qsTr("Default lambda = 1. Equivalent to a Beta binomial with a = 1 and b = lambda * p, where p is the number of predictors in the model.")
					childrenOnSameRow: true
					childrenArea.columnSpacing: 1
					DoubleField { name: "wilsonParamLambda"; label: qsTr("λ"); defaultValue: 1; inclusive: JASP.None; min: 0}
				}
				RadioButton
				{
					value: "castillo"
					label: qsTr("Castillo")
					info: qsTr("Default u = 1. Equivalent to a Beta binomial with a = 1 and b = p^u, where p is the number of predictors in the model.")
					childrenOnSameRow: true
					childrenArea.columnSpacing: 1
					DoubleField { name: "castilloParamU"; label: qsTr("u"); defaultValue: 1; inclusive: JASP.MinMax; min: 1}
				}
				RadioButton
				{
					value: "bernoulli"; label: qsTr("Bernoulli"); info: qsTr("Bernoulli prior. Default p = 0.5.")
					childrenOnSameRow: true
					DoubleField { name: "bernoulliParam"; label: qsTr("p"); defaultValue: 0.5; max: 1; inclusive: JASP.None; decimals: 3 }
				}
			}

			RadioButtonGroup
			{
				name: "samplingMethod"
				title: qsTr("Sampling Method"); info: qsTr("Indicates the sampling method to be used. It is recommended to use BAS when the model space can be enumerated.")
				RadioButton
				{
					value: "bas"; label: qsTr("BAS"); info: qsTr("Uses Bayesian Adaptive Sampling (without replacement). These can be updated based on estimates of the marginal inclusion. No. models indicates the number of models to sample without replacement. Setting the value to 0 implies the analysis will attempt to enumerate all models.") ;checked: true
					childrenOnSameRow: true
					IntegerField { name: "numberOfModels"; label: qsTr("No. models"); defaultValue: 0; max: 100000000 }
				}
				RadioButton
				{
					value: "mcmc"; label: qsTr("MCMC"); info: qsTr("Samples with replacement via an MCMC algorithm that combines the birth/death random walk with a random swap move to interchange a variable in the model. No. samples indicates the number of MCMC samples to draw. Setting the value to 0 implies the number of MCMC iterations is equal to 10 times the number of models. Sampling stops when min(number of models, MCMC iterations) is reached.")
					childrenOnSameRow: true
					IntegerField { name: "samples"; label: qsTr("No. samples"); defaultValue: 0; max: 100000000 }
				}
			}

			Group
			{
				title: qsTr("Numerical Accuracy"); info: qsTr("Accuracy of credible intervals. There is an option to select the number of samples used to estimate the credible interval. The more samples the more accurate the credible intervals will be.")
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

	Common.ExportBayesian 
	{ 
		summaryTypeValue: summaryType.currentLabel 
	}	

}
