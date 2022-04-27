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
Form {
    id: form
    function updateWeightsTitle() {
        if (family.currentText == "Binomial")
        {title = qsTr("Total Number of Trials")}
        else
        {title = qsTr("Weights")}
        return title
    }

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
            allowedColumns:		["scale","ordinal", "nominal", "nominalText"]
            singleVariable:		true
        }

        AssignedVariablesList
        {
            name:				"covariates"
            title:				qsTr("Covariates")
            allowedColumns:		["ordinal", "scale"]
        }

        AssignedVariablesList
        {
            name:				"factors"
            title:				qsTr("Factors")
            allowedColumns:		["scale", "ordinal", "nominal", "nominalText"]
        }

        AssignedVariablesList
        {
            name:              "weights"
            title:             updateWeightsTitle();
            allowedColumns:    ["ordinal", "scale"]
            singleVariable:    true
        }
    }

    Group
    {

        DropDown
        {
            name:				"family"
            label:				qsTr("Family")
            id:					family
            indexDefaultValue:	0
            values:
            [
                { label: qsTr("Bernoulli"),				value: "bernoulli"},
                { label: qsTr("Binomial"),	            value: "binomial"},
                { label: qsTr("Gaussian"),				value: "gaussian"},
                { label: qsTr("Gamma"),					value: "Gamma"},
                { label: qsTr("Inverse Gaussian"),		value: "inverse.gaussian"},
                { label: qsTr("Poisson"),				value: "poisson"}
            ]

            property var familyMap:
            {
                "bernoulli":		["logit", "probit", "cauchit", "log", "cloglog"],
                "binomial":         ["logit", "probit", "cauchit", "log", "cloglog"],
                "gaussian":			["identity", "log", "inverse"],
                "Gamma":			["identity", "log", "inverse"],
                "inverse.gaussian":	["identity", "log", "inverse", "1/mu^2"],
                "poisson":			["identity", "log", "sqrt"]
            }

            property var familyDefault:
            {
                "bernoulli":		"logit",
                "binomial":		    "logit",
                "gaussian":			"identity",
                "Gamma":			"inverse",
                "inverse.gaussian":	"1/mu^2",
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

            RadioButton
            {
                label:		qsTr("Logit")
                value:		"logit"
                visible:	family.familyMap[family.currentValue].includes(value)
                checked:	family.familyDefault[family.currentValue] == "logit"
            }

            RadioButton
            {
                label:		qsTr("Probit")
                value:		"probit"
                visible:	family.familyMap[family.currentValue].includes(value)
                checked:	family.familyDefault[family.currentValue] == "probit"
            }

            RadioButton
            {
                label:		qsTr("Cauchit")
                value:		"cauchit"
                visible:	family.familyMap[family.currentValue].includes(value)
                checked:	family.familyDefault[family.currentValue] == "cauchit"
            }

            RadioButton
            {
                label:		qsTr("Complementary LogLog")
                value:		"cloglog"
                visible:	family.familyMap[family.currentValue].includes(value)
                checked:	family.familyDefault[family.currentValue] == "cloglog"
            }

            RadioButton
            {
                label:		qsTr("Identity")
                value:		"identity"
                visible:	family.familyMap[family.currentValue].includes(value)
                checked:	family.familyDefault[family.currentValue] == "identity"
            }

            RadioButton
            {
                label:		qsTr("Log")
                value:		"log"
                visible:	family.familyMap[family.currentValue].includes(value)
                checked:	family.familyDefault[family.currentValue] == "log"
            }

            RadioButton
            {
                label:		qsTr("Sqrt")
                value:		"sqrt"
                visible:	family.familyMap[family.currentValue].includes(value)
                checked:	family.familyDefault[family.currentValue] == "sqrt"
            }

            RadioButton
            {
                label:		qsTr("Inverse")
                value:		"inverse"
                visible:	family.familyMap[family.currentValue].includes(value)
                checked:	family.familyDefault[family.currentValue] == "inverse"
            }

            RadioButton
            {
                label:		qsTr("1/mu^2")
                value:		"1/mu^2"
                visible:	family.familyMap[family.currentValue].includes(value)
                checked:	family.familyDefault[family.currentValue] == "1/mu^2"
            }
        }
    }

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

        Group
        {
            title: qsTr("Analysis of Residuals")
            Layout.rowSpan: 3
            Group
            {
                title: qsTr("Deviance Residuals")
                CheckBox {
                    name: "devResVsYPlot"; label: qsTr("Residuals vs. fitted")
                }
                CheckBox {
                    name: "devResVsXPlot"; label: qsTr("Residuals vs. predictor")
                }
                CheckBox {
                    name: "devResQqPlot"; label: qsTr("Q-Q plot")
                }
            }

            Group
            {
                title: qsTr("Pearson Residuals")
                CheckBox {
                    name: "prsResVsYPlot"; label: qsTr("Residuals vs. fitted")
                }
                CheckBox {
                    name: "prsResVsXPlot"; label: qsTr("Residuals vs. predictor")
                }
                CheckBox {
                    name: "prsResQqPlot"; label: qsTr("Q-Q plot")
                }

            }

            Group
            {
                title: qsTr("Quantile Residuals")
                CheckBox {
                    name: "quanResVsYPlot"; label: qsTr("Residuals vs. fitted")
                }
                CheckBox {
                    name: "quanResVsXPlot"; label: qsTr("Residuals vs. predictor")
                }
                CheckBox {
                    name: "quanResQqPlot"; label: qsTr("Q-Q plot")
                }
            }

            Group
            {
                title: qsTr("Other Plots")
                CheckBox {
                    name: "partialPlot"; label: qsTr("Partial residual plots")
                }
                CheckBox {
                    name: "zVsEtaPlot"; label: qsTr("Working responses vs. linear predictor")
                }
            }
        }

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


        CheckBox {
            name: "marginalMeansCi"; label: qsTr("Confidence interval")
            childrenOnSameRow: true
            CIField { name: "marginalMeansCiWidth"; }
        }

        DoubleField
        {
            id:				marginalMeansSD
            name:			"marginalMeansSd"
            label:			qsTr("Levels of covariates at mean +/- ")
            defaultValue: 	1
            min:			0
            enabled:		marginalMeansVars.columnsTypes.includes("scale")
            afterLabel:     "SD"
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
            name: "marginalMeansResponse"
            label: qsTr("Use response scale")
            checked: true
        }

        CheckBox
        {
            name:	"marginalMeansContrast"
            id:		marginalMeansContrast
            label:	qsTr("Specify contrasts")
        }

        DropDown
        {
            name:	"emmPAdjustment"
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
}

