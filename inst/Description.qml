import QtQuick
import JASP.Module

Description
{
	title		: qsTr("Regression")
	description	: qsTr("Evaluate the association between variables")
	icon		: "analysis-classical-regression.svg"
	hasWrappers	: true

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
	Analysis
	{
		menu:	qsTr("Logistic Regression")
		title:	qsTr("Bayesian Logistic Regression")
		func:	"RegressionLogisticBayesian"
	}
}
