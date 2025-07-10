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

// All Analysis forms must be built with the From QML item
Group {
	property bool otherFamilyNotSelected: family.currentValue !== "other";

	VariablesForm
	{
		preferredHeight: 350

		AvailableVariablesList
		{
			name:				"allVariablesList"
		}

		AssignedVariablesList
		{
			name:				"dependent"
			title:				qsTr("Dependent variable")
			info: qsTr("The response variable.")
			allowedColumns:		(family.currentValue !== "other") ? "scale" : ["scale", "ordinal", "nominal"]
			singleVariable:		true
		}

		AssignedVariablesList
		{
			name:				"covariates"
			title:				qsTr("Covariates")
			info: qsTr("Quantitative variables, such as age, height and IQ.")
			allowedColumns:		["scale"]
		}

		AssignedVariablesList
		{
			name:				"factors"
			title:				qsTr("Factors")
			info: qsTr("Qualitative variables, such as gender and group memberships.")
			allowedColumns:		["nominal"]
		}

		AssignedVariablesList
		{
			name:              "weights"
			title:             family.currentValue === "binomial" ? qsTr("Total Number of Trials") : qsTr("Weights")
			info: qsTr("Prior weights of the model. Mandatory when the binomial family is selected. In this case, the name changes to total Number of Trials, and the dependent variable now refers to the proportion of success (between 0 and 1).")
			allowedColumns:    ["scale"]
			singleVariable:    true
		}

		AssignedVariablesList
		{
			name:              "offset"
			title:             qsTr("Offset")
			allowedColumns:    ["scale"]
			singleVariable:    true
		}
	}

	Group
	{

		DropDown
		{
			name:				"family"
			label:				qsTr("Family")
			info: qsTr("Distribution of the response variable:")
			id:					family
			indexDefaultValue:	0
			values:
				[
				{ label: qsTr("Bernoulli"),	     		value: "bernoulli"},
				{ label: qsTr("Binomial"),	            value: "binomial"},
				{ label: qsTr("Gaussian"),				value: "gaussian"},
				{ label: qsTr("Gamma"),					value: "gamma"},
				{ label: qsTr("Inverse Gaussian"),		value: "inverseGaussian"},
				{ label: qsTr("Poisson"),				value: "poisson"},
				{ label: qsTr("Other"),				    value: "other"}
			]

			property var familyMap:
			{
				"bernoulli":		["logit", "probit", "cauchit", "log", "cloglog"],
				"binomial":         ["logit", "probit", "cauchit", "log", "cloglog"],
				"gaussian":			["identity", "log", "inverse"],
				"gamma":			["identity", "log", "inverse"],
				"inverseGaussian":	["identity", "log", "inverse", "1/mu^2"],
				"poisson":			["identity", "log", "sqrt"]
			}

			property var familyDefault:
			{
				"bernoulli":		"logit",
				"binomial":		    "logit",
				"gaussian":			"identity",
				"gamma":			"inverse",
				"inverseGaussian":	"1/mu^2",
				"poisson":			"log"
			}

			onCurrentValueChanged:
			{
				if (!familyMap[currentValue].includes(link.value))
				{
					for (var i = 0; i < link.buttons.length; i++)
						if (familyMap[currentValue].includes(link.buttons[i].parent.value))
						{
							link.buttons[i].parent.click()
							break;
						}
				}
			}
		}

		RadioButtonGroup
		{
			id:						link
			name:					"link"
			title:					qsTr("Link")
			radioButtonsOnSameRow:	true
			visible:                family.currentValue !== "other"

			RadioButton
			{
				label:		qsTr("Logit")
				value:		"logit"
				visible:	family.familyMap[family.currentValue].includes(value)
				checked:	family.familyDefault[family.currentValue] === "logit"
			}

			RadioButton
			{
				label:		qsTr("Probit")
				value:		"probit"
				visible:	family.familyMap[family.currentValue].includes(value)
				checked:	family.familyDefault[family.currentValue] === "probit"
			}

			RadioButton
			{
				label:		qsTr("Cauchit")
				value:		"cauchit"
				visible:	family.familyMap[family.currentValue].includes(value)
				checked:	family.familyDefault[family.currentValue] === "cauchit"
			}

			RadioButton
			{
				label:		qsTr("Complementary LogLog")
				value:		"cloglog"
				visible:	family.familyMap[family.currentValue].includes(value)
				checked:	family.familyDefault[family.currentValue] === "cloglog"
			}

			RadioButton
			{
				label:		qsTr("Identity")
				value:		"identity"
				visible:	family.familyMap[family.currentValue].includes(value)
				checked:	family.familyDefault[family.currentValue] === "identity"
			}

			RadioButton
			{
				label:		qsTr("Log")
				value:		"log"
				visible:	family.familyMap[family.currentValue].includes(value)
				checked:	family.familyDefault[family.currentValue] === "log"
			}

			RadioButton
			{
				label:		qsTr("Sqrt")
				value:		"sqrt"
				visible:	family.familyMap[family.currentValue].includes(value)
				checked:	family.familyDefault[family.currentValue] === "sqrt"
			}

			RadioButton
			{
				label:		qsTr("Inverse")
				value:		"inverse"
				visible:	family.familyMap[family.currentValue].includes(value)
				checked:	family.familyDefault[family.currentValue] === "inverse"
			}

			RadioButton
			{
				label:		qsTr("1/mu^2")
				value:		"1/mu^2"
				visible:	family.familyMap[family.currentValue].includes(value)
				checked:	family.familyDefault[family.currentValue] === "1/mu^2"
			}
		}

		DropDown
		{
			name:				"otherGlmModel"
			label:				qsTr("Model")
			id:					otherGlmModel
			visible:            family.currentValue === "other"
			indexDefaultValue:	0
			values:
				[
				{ label: qsTr("Multinomial Logistic Regression"),		value: "multinomialLogistic"},
				{ label: qsTr("Ordinal Logistic Regression"),	        value: "ordinalLogistic"},
				{ label: qsTr("Firth Logistic Regression"),				value: "firthLogistic"}
			]
		}
	}
}

