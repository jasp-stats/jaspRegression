//
// Copyright (C) 2013-2022 University of Amsterdam
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

Section
{
	title: qsTr("Export")
	property string summaryTypeValue

	Group
	{
		title: qsTr("Append columns to data")
		Layout.columnSpan: 2
		CheckBox
		{
			id:							residualsSavedToData
			name:						"residualsSavedToData"
			text:						qsTr("Residuals (%1)").arg(summaryTypeValue)
			info: qsTr("Appends the posterior mean of the residuals as a new column in the data file.")


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
			text:						qsTr("Residuals std. deviations (%1)").arg(summaryTypeValue)
			info: qsTr("Appends the posterior standard deviation of the residuals as a new column in the data file.")


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

}
