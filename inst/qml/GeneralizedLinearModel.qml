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

import QtQuick			2.8
import QtQuick.Layouts  1.3
import JASP.Controls	1.0
import JASP.Widgets		1.0
import JASP				1.0
import "./common"		as GLM

// All Analysis forms must be built with the From QML item
Form {

	GLM.GlmInputComponent {}

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
				source: ['covariates', 'factors']
				width: parent.width / 4
			}
			ModelTermsList { width: parent.width * 5 / 9 }
		}

		CheckBox { name: "interceptTerm"; label: qsTr("Include intercept"); checked: true }
	}

	Section
	{
		title: qsTr("Statistics")

		Group
		{
			title: qsTr("Model Fit")
			CheckBox {
				name: "gofDeviance"; label: qsTr("Deviance goodness-of-fit test")
			}
			CheckBox {
				name: "gofPearson"; label: qsTr("Pearson goodness-of-fit test")
			}
		}

		Group
		{
			title: qsTr("Parameter Estimates")
			CheckBox { name: "coefEstimates"; label: qsTr("Estimates"); checked: true }
			CheckBox {
				name: "coefCi"; label: qsTr("Confidence intervals")
				childrenOnSameRow: true
				CIField { name: "coefCiInterval"; }
			}
		}


	}

	Section
	{
		title: qsTr("Diagnostics")

		GLM.GlmResidualAnalysisPlotsComponent {}

		Group {
			title: qsTr("Show Outliers")
			CheckBox {
				name: "outlierQuanTable"; label: qsTr("Standardized quantile residuals: top")
				childrenOnSameRow: true
				IntegerField { name: "outlierQuanTableTopN"; defaultValue: 3}
			}
			CheckBox {
				name: "outlierStdTable"; label: qsTr("Standardized deviance residuals: top")
				childrenOnSameRow: true
				IntegerField { name: "outlierStdTableTopN"; defaultValue: 3}
			}
			CheckBox {
				name: "outlierStuTable"; label: qsTr("Studentized deviance residuals: top")
				childrenOnSameRow: true
				IntegerField { name: "outlierStuTableTopN"; defaultValue: 3}
			}
		}

		Group {
			title: qsTr("Show Influential Cases")
			CheckBox {
				name: "dfbetas"; label: qsTr("DFBETAS")
			}
			CheckBox {
				name: "dffits"; label: qsTr("DFFITS")
			}
			CheckBox {
				name: "covRatio"; label: qsTr("Covariance ratio")
			}
			CheckBox {
				name: "cooksD"; label: qsTr("Cook's distance")
			}
			CheckBox {
				name: "leverage"; label: qsTr("Leverages")
			}
		}

		Group {
			title: qsTr("Multicollinearity")
			CheckBox { name: "tolerance"; label: qsTr("Tolerance") }
			CheckBox { name: "vif"; label: qsTr("VIF") }
		}
	}

	GLM.EmmComponent {}
}
