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
	title: qsTr("Residuals"); 
	CheckBox 
	{ 
		name: "residualStatistic"     
		label: qsTr("Statistics") ; info: qsTr("Displays descriptive statistics of the residuals and predicted values.")
		visible: analysis === Type.Analysis.LinearRegression
	}
	
	CheckBox
	{
		name: "residualCasewiseDiagnostic"
		label: qsTr("Casewise diagnostics")
		info: qsTr("Provides casewise diagnostics for residuals, displaying cases above a threshold based on standardized residuals (default: 3) or Cook's distance (default: 1), or displaying all cases.")
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
				value: "cooksDistance";	label: qsTr("Cook's dist. >")
				childrenOnSameRow: true
				DoubleField { name: "residualCasewiseDiagnosticCooksDistanceThreshold";	defaultValue: 1	}
			}
			RadioButton { value: "allCases"; label: qsTr("All")								}
		}

		Group
		{
			CheckBox { name: "dfbetas"; 		label: qsTr("DFBETAS"); info: qsTr("The difference between a parameter estimated using all cases and estimated when one case is excluded. Indicated in the table when the absolute value of DFBETAS is greater than 1.")		}
			CheckBox { name: "dffits";   		label: qsTr("DFFITS"); info: qsTr("The difference between the predicted value for a case when the model is estimated including or excluding that case. Indicated in the table when the absolute value of DFFITS is greater than 3 * sqrt(k/(n-k)).")		}
			CheckBox { name: "covarianceRatio"; label: qsTr("Cov ratio"); info: qsTr("The degree to which a case influences the variance of the regression parameters. Indicated in the table when the covariance ratio is greater than 3 * k/(n-k).")	}
			CheckBox { name: "leverage"; 		label: qsTr("Leverage"); info: qsTr("The influence of the observed value of the outcome variable over the predicted values. Indicated in the table when the leverage is greater than 3 * k/n.")	}
			CheckBox { name: "mahalanobis"; 	label: qsTr("Mahalanobis"); info: qsTr("Measures the distance of cases from the mean(s) of the predictor variable(s).")	}

		}

	}

}