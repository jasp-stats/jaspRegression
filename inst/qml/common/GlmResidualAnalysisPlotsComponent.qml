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

// All Analysis forms must be built with the From QML item

Group
{
	title: qsTr("Analysis of Residuals")
	Layout.rowSpan: 3
	Group
	{
		title: qsTr("Deviance Residuals")
		CheckBox { name: "devianceResidualVsFittedYPlot";	label: qsTr("Residuals vs. fitted")		}
		CheckBox { name: "devianceResidualVsXPlot";	label: qsTr("Residuals vs. predictor")	}
		CheckBox { name: "devianceResidualQqPlot";	label: qsTr("Q-Q plot")					}
	}

	Group
	{
		title: qsTr("Pearson Residuals")
		CheckBox { name: "pearsonResidualVsFittedYPlot";	label: qsTr("Residuals vs. fitted") 	}
		CheckBox { name: "pearsonResidualVsXPlot";	label: qsTr("Residuals vs. predictor")	}
		CheckBox { name: "pearsonResidualQqPlot";	label: qsTr("Q-Q plot")					}

	}

	Group
	{
		title: qsTr("Quantile Residuals")
		CheckBox { name: "quantileResidualVsFittedYPlot";	label: qsTr("Residuals vs. fitted")		}
		CheckBox { name: "quantileResidualVsFittedYPlot";	label: qsTr("Residuals vs. predictor")	}
		CheckBox { name: "quantileResidualQqPlot";	label: qsTr("Q-Q plot")					}
	}

	Group
	{
		title: qsTr("Other Plots")
		CheckBox { name: "partialResidualPlot";	label: qsTr("Partial residual plots")					}
		CheckBox { name: "zVsEtaPlot";	label: qsTr("Working responses vs. linear predictor")	}
	}
}

