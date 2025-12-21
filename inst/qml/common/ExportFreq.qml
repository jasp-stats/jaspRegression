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
import JASP
import JASP.Controls
import "./" as Classical


Section
{
	title: qsTr("Export")

	Group
	{
		CheckBox
		{
			id:							residualsSavedToData
			name:						"residualsSavedToData"
			info: qsTr("Save the residuals of the most complex model as a new column in the data file.")
			text:						qsTr("Append residuals to data")

			ComputedColumnField
			{
				name:					"residualsSavedToDataColumn"
				text:					qsTr("Column name")
				placeholderText:		qsTr("e.g., residuals")
				fieldWidth:				120
				enabled:				residualsSavedToData.checked
			}

			RadioButtonGroup
			{
				title: qsTr("Residuals type")
				name: "residualsSavedToDataType"
				info: qsTr("Choose the type of residual to be appended. Raw residuals are simply the differences between observation and model prediction, standardized residuals divide each residual by an estimate of its standard deviation (using rstandard()), whereas studentized residuals divide each residual by an unbiased estimate of its standard deviation computed by refitting the model without that observation  (using rstudent()).")
				RadioButton { value: "raw";			label: qsTr("Raw"); checked: true	}
				RadioButton { value: "standard";	label: qsTr("Standardized")		}
				RadioButton { value: "student";		label: qsTr("Studentized")		}
			}
		}

		CheckBox
		{
			id:							predictionsSavedToData
			name:						"predictionsSavedToData"
			info: qsTr("Save the predictions of the most complex model as a new column in the data file")
			text:						qsTr("Append predictions to data")

			ComputedColumnField
			{
				name:					"predictionsSavedToDataColumn"
				text:					qsTr("Column name")
				placeholderText:		qsTr("e.g., predictions")
				fieldWidth:				120
				enabled:				predictionsSavedToData.checked
			}
		}
	}
}
