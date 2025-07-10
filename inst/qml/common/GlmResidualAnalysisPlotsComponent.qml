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
	title: qsTr("Analysis of Residuals"); info: qsTr("The following options provide visual diagnostics of the model assumptions based on residuals.")
	Layout.rowSpan: 3
	Group
	{
		title: qsTr("Deviance Residuals"); info: qsTr("Provides different types of diagnostic plots based on deviance residuals.")
		CheckBox { name: "devianceResidualVsFittedPlot";	label: qsTr("Residuals vs. fitted"); info: qsTr("Displays a scatter plot of the standardized deviance residuals against the fitted values (with constant-information scale transformations).")	}
		CheckBox { name: "devianceResidualVsPredictorPlot";	label: qsTr("Residuals vs. predictor"); info: qsTr("Displays a scatter plot of the standardized deviance residuals against every predictor on its original scale.")	}
		CheckBox { name: "devianceResidualQqPlot";	label: qsTr("Q-Q plot")		; info: qsTr("Displays a Q-Q plot of the standardized deviance residuals.")		}
	}

	Group
	{
		title: qsTr("Pearson Residuals"); info: qsTr("Provides different types of diagnostic plots based on Pearson residuals.")
		CheckBox { name: "pearsonResidualVsFittedPlot";	label: qsTr("Residuals vs. fitted"); info: qsTr("Displays a scatter plot of the standardized Pearson residuals against the fitted values (with constant-information scale transformations).") 	}
		CheckBox { name: "pearsonResidualVsPredictorPlot";	label: qsTr("Residuals vs. predictor"); info: qsTr("Displays a scatter plot of the standardized Pearson residuals against every predictor on its original scale.")	}
		CheckBox { name: "pearsonResidualQqPlot";	label: qsTr("Q-Q plot"); info: qsTr("Displays a Q-Q plot of the standardized Pearson residuals.")			}

	}

	Group
	{
		title: qsTr("Quantile Residuals"); info: qsTr("Provides different types of diagnostic plots based on quantile residuals. Highly recommended for discrete families (e.g., Bernoulli, Binomial, Poisson).")
		CheckBox { name: "quantileResidualVsFittedPlot";	label: qsTr("Residuals vs. fitted"); info: qsTr("Displays a scatter plot of the standardized quantile residuals against the fitted values (with constant-information scale transformations).")		}
		CheckBox { name: "quantileResidualVsPredictorPlot";	label: qsTr("Residuals vs. predictor"); info: qsTr("Displays a scatter plot of the standardized quantile residuals against every predictor on its original scale.")	}
		CheckBox { name: "quantileResidualQqPlot";	label: qsTr("Q-Q plot"); info: qsTr("Displays a Q-Q plot of the standardized quantile residuals.")					}
	}

	Group
	{
		title: qsTr("Other Plots")
		CheckBox { name: "partialResidualPlot";	label: qsTr("Partial residual plots"); info: qsTr("Displays partial residual plots across predictors.")					}
		CheckBox { name: "workingResponseVsLinearPredictorPlot";	label: qsTr("Working responses vs. linear predictor"); info: qsTr("Displays a scatter plot of the model's working responses z against the predicted linear predictor values.")	}
	}
}

