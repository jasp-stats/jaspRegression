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

// All Analysis forms must be built with the From QML item
Form
{   
	info: qsTr("A generalized linear model (GLM) is a flexible extension of ordinary linear regression. A widely used GLM is binary logistic regression. Generally speaking, a GLM consists of a random component and a systematic component:\n") +
    " The random component specifies an appropriate probability distribution for the response variable. For example, a binomial distribution is appropriate for proportions of a total.\n" +
    " The systematic component specifies how the explanatory variables relate to the mean of the response. For example, in binary logistic regression, the logit link function is used to map the responses (i.e. probabilities) to the linear combination of predictors (i.e. linear predictor).\n" +
	"## " + qsTr("Family and link") + "\n" + "The following table summarizes the available distributions, also called families, and link functions, as well as the suitable type of response data. The asterisk * indicates the canonical/default link function for a specific family.\n" + "\n" + 
	"- Bernoulli\n" + "\t" + "- Links: Logit*, Probit, Cauchit, Complementary log-log, log.\n" + "\t" + "- Response type: Proportions, counts.\n" + 
	"- Binomial\n" + "\t" + "- Links: Logit*, Probit, Cauchit, Complementary Log-Log, Log.\n" + "\t" + "- Response type: Proportions, counts.\n" +
	"- Gaussian\n" + "\t" + "- Links: Identity*, Log, Inverse.\n" + "\t" + "- Response type: Continuous.\n" +
	"- Gamma\n" + "\t" + "- Links: Identity, Log, Inverse*.\n" + "\t" + "- Response type: Positive continuous.\n" +
	"- Inverse Gaussian\n" + "\t" + "- Links: Identity, Log, Inverse, 1/mu^2*.\n" + "\t" + "- Response type: Positive continuous.\n" +
	"- Poisson\n" + "\t" + "- Identity, Log*, Square-root.\n" + "\t" + "- Response type: Counts.\n" +
	"## " + qsTr("Assumptions") + "\n" + "- Lack of outliers: All responses were generated from the same process, so that the same model is appropriate for all the observations.\n" +
    "- Independence: The responses are independent of each other.\n" + "- Distribution: The responses come from the specified EDM.\n" + "- Link function: The correct link function is used.\n" +
	"- Linearity: All important explanatory variables are included, and each explanatory variable is included in the linear predictor on the correct scale.\n" + "- Variance function: The correct variance function is used.\n" +
	"- Dispersion parameter: The dispersion parameter is constant.\n" + "## " + qsTr("Input") 
	
	id: form
	property int analysis:	Common.Type.Analysis.GLM
	property int framework:	Common.Type.Framework.Classical

	Formula
	{
		lhs: "dependent"
		rhs: [{ name: "modelTerms", extraOptions: "isNuisance" }]
		userMustSpecify: "covariates"
	}

	Common.GlmInputComponent {id: input}

	Section
	{   
		title: qsTr("Model")

		VariablesForm
		{
			preferredHeight: jaspTheme.smallDefaultVariablesFormHeight

			AvailableVariablesList
			{
				name: 	"availableTerms"
				title: 	qsTr("Components")
				info: qsTr("All the independent variables and covariates that can be included in the model.") 
				source: ['covariates', 'factors']
				width: 	parent.width / 4
			}
			ModelTermsList { width: parent.width * 5 / 9 }
		}

		CheckBox
		{
			name:		"interceptTerm"
			label:		qsTr("Include intercept")
			info: qsTr("Selected by default. This adds an intercept term to the model.")
			checked:	true
		}
	}

	Section
	{
		title: qsTr("Statistics")

		Group
		{
			title: qsTr("Model Fit"); info: qsTr("A table providing information about the goodness-of-fit of the model, including the corresponding fit statistic, the degree of freedom (df) and the p-value.")
			CheckBox { name: "devianceGoodnessOfFit";	label: qsTr("Deviance goodness-of-fit test"); info: qsTr("Goodness-of-fit test based on deviance residuals, comparing the current model against the saturated model.")}
			CheckBox { name: "pearsonGoodnessOfFit";	label: qsTr("Pearson goodness-of-fit test"); info: qsTr("Goodness-of-fit test based on Pearson residuals, comparing the current model against the saturated model.")	}
		}

		Group
		{
			title: qsTr("Coefficients")
			CheckBox
			{
				name:		"coefficientEstimate"
				label:		qsTr("Estimates")
				info: qsTr("Selected by default. This gives a table summarizing the model's parameter estimates, standard error around the parameter estimates, the test statistic (t or z), and the p-value.")
				checked:	true
			}
			CheckBox {
				name:				"coefficientCi"
				label:				qsTr("Confidence intervals")
				info: qsTr("Provide the confidence intervals around the parameter estimates. The level of the confidence intervals can be specified (default is 95%).")
				childrenOnSameRow:	true
				CIField { name: "coefficientCiLevel" }
			}
		}
	}

	Section
	{
		title: qsTr("Diagnostics")
		enabled: input.otherFamilyNotSelected

		Common.OutlierComponent { id: outlierComponent}

		Common.GlmResidualAnalysisPlotsComponent {}

		Group
		{
			title: qsTr("Show Outliers"); info: qsTr("A table showing the top n cases, ranked descendingly based on the size of the residuals. Note that the shown cases are not necessarily outliers. The column Case Number refers to the row number of the observation in the data set.")
			CheckBox
			{
				name:				"quantileResidualOutlierTable"
				label:				qsTr("Standardized quantile residuals : top")
				info: qsTr("Top n standardized quantile residuals.")
				childrenOnSameRow:	true
				IntegerField { name: "quantileResidualOutlierTableTopN"; defaultValue: 3	}
			}
			CheckBox
			{
				name:				"standardizedResidualOutlierTable"
				label:				qsTr("Standardized deviance residuals: top")
				info: qsTr("Top n standardized deviance residuals.")
				childrenOnSameRow:	true
				IntegerField { name: "standardizedResidualOutlierTableTopN"; defaultValue: 3		}
			}
			CheckBox
			{
				name:				"studentizedResidualOutlierTable"
				label:				qsTr("Studentized deviance residuals: top")
				info: qsTr("Top n studentized deviance residuals.")
				childrenOnSameRow:	true
				IntegerField { name: "studentizedResidualOutlierTableTopN"; defaultValue: 3		}
			}
		}



		Group
		{
			title: qsTr("Multicollinearity"); info: qsTr(" A table showing multicollinearity diagnostics of the model. The choices of measures are as follows.")
			CheckBox { name: "tolerance";	label: qsTr("Tolerance"); info: qsTr("Indicates how much of a predictor’s variance is not explained by other predictors.")	}
			CheckBox { name: "vif";			label: qsTr("VIF")	; info: qsTr("Variance inflation factor, which indicates how much variance of a predictor is inflated by multicollinearity among the predictor variables.")		}
		}
	}

	Common.EmmComponent { enabled: input.otherFamilyNotSelected }

	Section
	{
		title: qsTr("Advanced Options")
		SetSeed {}
		info: qsTr("Set seeds: This gives you the possibility of setting a random seed for the plots and tables based on quantile residuals in the Diagnostics Section. Concretely, every time you generate a plot or a table based on quantile residuals, you get a slightly different plot or table because one step in the calculation of the quantile residuals involves drawing random values from a distribution. By setting a random seed of your choice here (the default is 1), you make sure that you can get exactly the same plots and tables based on quantile residuals.")
		enabled: input.otherFamilyNotSelected
	}

	Common.ExportFreq 
	{  
		enabled: input.otherFamilyNotSelected 
		id: exportComponent
	}
}

