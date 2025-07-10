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

// All Analysis forms must be built with the From QML item
Section
{
	title:		qsTr("Estimated Marginal Means and Contrast Analysis")
	expanded:	false

	VariablesForm
	{
		preferredHeight:	jaspTheme.smallDefaultVariablesFormHeight

		AvailableVariablesList
		{
			name:	"availableModelComponentsMeans"
			title:	qsTr("Model variables")
			info: qsTr("Variables that can be used for computing estimated marginal means.")
			source: [{ name: "covariates", use: "noInteraction" }, { name: "factors", use: "noInteraction" }]
		}

		AssignedVariablesList
		{
			id:		marginalMeansVars
			name:	"marginalMeansVars"
			title:	qsTr("Selected variables")
			info: qsTr("Variables for which the estimated marginal means will be computed.")
		}
	}


	CheckBox
	{
		name: "marginalMeansCi"
		label: qsTr("Confidence interval")
		info: qsTr("Width/level of the confidence interval for the estimated marginal means.") 
		childrenOnSameRow: true
		CIField { name: "marginalMeansCiWidth" }
	}

	DoubleField
	{
		id:				marginalMeansSD
		name:			"marginalMeansSd"
		label:			qsTr("Levels of covariates at mean +/- ")
		defaultValue: 	1
		min:			0
		enabled:		marginalMeansVars.columnsTypes.includes("scale")
		afterLabel:     qsTr("SD")
	}

	Group
	{
		columns: 2

		CheckBox
		{
			name:	"marginalMeansComparison"
			id:		marginalMeansCompare
			label:	qsTr("Compare marginal means to:")
			info: qsTr("Value to which the estimated marginal means will be compared to. The default value is set to 0 and can be changed by the user.")
		}

		DoubleField
		{
			enabled:	marginalMeansCompare.checked
			name:		"marginalMeansComparisonWith"
		}
	}

	CheckBox
	{
		name:		"marginalMeansResponse"
		label:		qsTr("Use response scale")
		info: qsTr("Decide whether the estimated marginal means should be computed on the response scale or the untransformed linear scale. The former is selected by default.")
		checked:	true
	}

	CheckBox
	{
		name:	"marginalMeansContrast"
		id:		marginalMeansContrast
		label:	qsTr("Specify contrasts")
		info: qsTr("Create a table for specifying contrasts based on the estimated marginal means. The row indices correspond to the 'Level' column in the estimated marginal means output table. Columns with variable names contain the (combinations of) variable levels for each estimated marginal mean. Columns named Contrast 1, 2, ... are used for specifying the contrasts. To set a contrast between two marginal means, enter -1 and 1 to the corresponding rows. Interactions can be tested by specifying differences between the changes in marginal means of one variable across levels of another variable.")
	}

	DropDown
	{
		name:	"marginalMeansPAdjustment"
		label:	qsTr("P-value adjustment")
		info: qsTr("Only available when contrasts are specified. To correct for multiple comparison testing and avoid Type I errors, different methods for correcting the p-value are available:")
		values:
			[
			{ label: "Holm",	    			value: "holm"},
			{ label: qsTr("Multivariate-t"),	value: "mvt"},
			{ label: "Scheffé",					value: "scheffe"},
			{ label: "Tukey",					value: "tukey"},
			{ label: qsTr("None"),				value: "none"},
			{ label: "Bonferroni",				value: "bonferroni"},
			{ label: "Hommel",					value: "hommel"}
		]
		enabled:			marginalMeansContrast.checked
	}

	CustomContrastsTableView
	{
		Layout.columnSpan:	2
		visible:			marginalMeansContrast.checked
		name:				"contrasts"
		source:				"marginalMeansVars"
		scaleFactor:		marginalMeansSD.value
	}
}
