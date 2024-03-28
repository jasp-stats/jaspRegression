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
Group
{
	title: qsTr("Residuals")
	CheckBox 
	{ 
		name: "residualStatistic"     
		label: qsTr("Statistics") 
		visible: analysis === Type.Analysis.LinearRegression
	}
	
	CheckBox
	{
		name: "residualCasewiseDiagnostic";	label: qsTr("Casewise diagnostics")
		columns: 2
		RadioButtonGroup
		{
			name: "residualCasewiseDiagnosticType"
			RadioButton
			{
				value: "outliersOutside"; label: qsTr("Std. residual >"); checked: true
				childrenOnSameRow: true
				DoubleField { name: "residualCasewiseDiagnosticZThreshold"; defaultValue: 3	}
			}
			RadioButton
			{
				value: "cooksDistance";	label: qsTr("Cook's dist >")
				childrenOnSameRow: true
				DoubleField { name: "residualCasewiseDiagnosticCooksDistanceThreshold";	defaultValue: 1	}
			}
			RadioButton { value: "allCases"; label: qsTr("All")										}
		}

		Group
		{
			CheckBox { name: "dfbetas"; 		label: qsTr("DFBETAS")		}
			CheckBox { name: "dffits";   		label: qsTr("DFFITS")		}
			CheckBox { name: "covarianceRatio"; label: qsTr("Cov ratio")	}
			CheckBox { name: "leverage"; 		label: qsTr("Leverage")		}
			CheckBox { name: "mahalanobis"; 	label: qsTr("Mahalanobis")	}

		}

	}

	CheckBox
	{
		id:							residualsSavedToData
		name:						"residualsSavedToData"
		text:						qsTr("Append residuals to data")

		ComputedColumnField
		{
			name:					"residualsSavedToDataColumn"
			text:					qsTr("Column name")
			placeholderText:		qsTr("e.g., residuals")
			fieldWidth:				120
			enabled:				residualsSavedToData.checked
		}
	}
}