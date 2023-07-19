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
			source: [{ name: "covariates", use: "noInteraction" }, { name: "factors", use: "noInteraction" }]
		}

		AssignedVariablesList
		{
			id:		marginalMeansVars
			name:	"marginalMeansVars"
			title:	qsTr("Selected variables")
		}
	}


	CheckBox
	{
		name: "marginalMeansCi"
		label: qsTr("Confidence interval")
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
		checked:	true
	}

	CheckBox
	{
		name:	"marginalMeansContrast"
		id:		marginalMeansContrast
		label:	qsTr("Specify contrasts")
	}

	DropDown
	{
		name:	"marginalMeansPAdjustment"
		label:	qsTr("P-value adjustment")
		values:
			[
			{ label: "Holm",					value: "holm"},
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
