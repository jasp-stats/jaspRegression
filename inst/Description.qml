import QtQuick 		2.12
import JASP.Module 	1.0

Description
{
	name		: "jaspRegression"
	title		: qsTr("Regression")
	description	: qsTr("Evaluate the association between variables")
	icon		: "analysis-classical-regression.svg"
	version		: "0.17.0"
	author		: "JASP Team"
	maintainer	: "JASP Team <info@jasp-stats.org>"
	website		: "jasp-stats.org"
	license		: "GPL (>= 2)"

	GroupTitle
	{
		title:	qsTr("Classical")
		icon:	"analysis-classical-regression.svg"
	}
	Analysis
	{
		title:	qsTr("Correlation")
		func:	"Correlation"
	}
	Analysis
	{
		title:	qsTr("Linear Regression")
		func:	"RegressionLinear"
	}
	Analysis
	{
		title:	qsTr("Logistic Regression")
		func:	"RegressionLogistic"
	}
	Analysis
	{
		title: qsTr("Generalized Linear Model")
		func: "GeneralizedLinearModel"
	}

	Separator{}
	GroupTitle
	{
		title:	qsTr("Bayesian")
		icon:	"analysis-bayesian-regression.svg"
	}
	Analysis
	{
		menu:	qsTr("Correlation")
		title:	qsTr("Bayesian Correlation")
		func:	"CorrelationBayesian"
	}
	Analysis
	{
		menu:	qsTr("Linear Regression")
		title:	qsTr("Bayesian Linear Regression")
		func:	"RegressionLinearBayesian"
	}
}
