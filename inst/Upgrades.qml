
import QtQuick		2.12
import JASP.Module	1.0

Upgrades
{
	Upgrade
	{
		functionName:		"Correlation"
		fromVersion:		"0.15"
		toVersion:			"0.16.3"

		ChangeRename { from: "conditioningVariables";	to: "partialOutVariables" }
	}

	// option renaming for syntax
	Upgrade
	{
		functionName:		"Correlation"
		fromVersion:		"0.16.3"
		toVersion:			"0.16.4"

		// Main section
		ChangeRename { from: "displayPairwise";						to: "pairwiseDisplay"						}
		ChangeRename { from: "reportSignificance";					to: "significanceReport"					}
		ChangeRename { from: "flagSignificant";						to: "significanceFlagged"					}

		ChangeRename { from: "confidenceIntervals";					to: "ci"									}
		ChangeRename { from: "confidenceIntervalsInterval";			to: "ciLevel"								}

		ChangeRename { from: "bootstrap";							to: "ciBootstrap"							}
		ChangeRename { from: "bootstrapReplicates";					to: "ciBootstrapSamples"					}

		ChangeRename { from: "VovkSellkeMPR";						to: "vovkSellke"							}

		ChangeRename { from: "hypothesis";							to: "alternative"							}
		ChangeJS
		{
			name:		"alternative"
			jsFunction:	function(options)
			{
				switch(options["alternative"])
				{
					case "correlated":				return "twoSided"	;
					case "correlatedPositively":	return "greater"	;
					case "correlatedNegatively":	return "less"		;
				}
			}
		}

		// Plots
		ChangeRename { from: "plotCorrelationMatrix";				to: "scatterPlot"							}
		ChangeRename { from: "plotDensities";						to: "scatterPlotDensity"					}
		ChangeRename { from: "plotStatistics";						to: "scatterPlotStatistic"					}
		ChangeRename { from: "plotConfidenceIntervals";				to: "scatterPlotCi"							}
		ChangeRename { from: "plotConfidenceIntervalsInterval";		to: "scatterPlotCiLevel"					}
		ChangeRename { from: "plotPredictionIntervals";				to: "scatterPlotPredictionInterval"			}
		ChangeRename { from: "plotPredictionIntervalsInterval";		to: "scatterPlotPredictionIntervalLevel"	}
		ChangeRename { from: "plotHeatmap";							to: "heatmapPlot"							}

		// Assumption checks
		ChangeRename { from: "multivariateShapiro";					to: "assumptionCheckMultivariateShapiro"	}
		ChangeRename { from: "multivariateRoyston";					to: "assumptionCheckMultivariateRoyston"	}
		ChangeRename { from: "multivariateMardia";					to: "assumptionCheckMultivariateMardia"		}
		ChangeRename { from: "multivariateEnergy";					to: "assumptionCheckMultivariateEnergy"		}

		ChangeRename { from: "pairwiseShapiro";						to: "assumptionCheckPairwiseShapiro"		}
		ChangeRename { from: "pairwiseRoyston";						to: "assumptionCheckPairwiseRoyston"		}
		ChangeRename { from: "pairwiseMardia";						to: "assumptionCheckPairwiseMardia"			}
		ChangeRename { from: "pairwiseEnergy";						to: "assumptionCheckPairwiseEnergy"			}


		// Options
		ChangeRename { from: "meansAndStdDev";						to: "meansAndSd"							}
		ChangeRename { from: "missingValues";						to: "naAction"								}

		ChangeJS
		{
			name:		"naAction"
			jsFunction:	function(options)
			{
				switch(options["missingValues"])
				{
					case "excludePairwise":		return "pairwise";
					case "excludeListwise":		return "listwise";
					default:					return options["missingValues"];
				}
			}
		}
	}

	Upgrade
	{
		functionName:		"RegressionLinearBayesian"
		fromVersion:		"0.16.3"
		toVersion:			"0.16.4"

		ChangeRename { from: "wlsWeights";									to: "weights"									}
		ChangeRename { from: "postSummaryTable";							to: "posteriorSummaryTable"						}
		ChangeRename { from: "postSummaryPlot";								to: "posteriorSummaryPlot"						}
		ChangeRename { from: "omitIntercept";								to: "posteriorSummaryPlotWithoutIntercept"		}
		ChangeRename { from: "posteriorSummaryPlotCredibleIntervalValue";	to: "posteriorSummaryPlotCiLevel"				}
		ChangeRename { from: "shownModels";									to: "modelsShown"								}
		ChangeRename { from: "numShownModels";								to: "numModelsShown"							}

		ChangeRename { from: "plotInclusionProbabilities";					to:	"inclusionProbabilitiesPlot"				}
		ChangeRename { from: "plotCoefficientsPosterior";					to:	"marginalPosteriorPlot"						}
		ChangeRename { from: "plotLogPosteriorOdds";						to:	"logPosteriorOddsPlot"						}
		ChangeRename { from: "plotModelComplexity";							to:	"modelComplexityPlot"						}
		ChangeRename { from: "plotModelProbabilities";						to:	"modelProbabilitiesPlot"					}
		ChangeRename { from: "plotResidualsVsFitted";						to:	"residualsVsFittedPlot"						}
		ChangeRename { from: "plotQQplot";									to:	"qqPlot"									}

		ChangeRename { from: "addResiduals";								to:	"residualsSavedToData"						}
		ChangeRename { from: "addResidualSds";								to:	"residualSdsSavedToData"					}

		ChangeRename { from: "residualsColumn";								to:	"residualsSavedToDataColumn"				}
		ChangeRename { from: "residualSdsColumn";							to:	"residualSdsSavedToDataColumn"				}

		ChangeJS
		{
			name:		"priorRegressionCoefficients"
			jsFunction:	function(options)
			{
				switch(options["priorRegressionCoefficients"])
				{
					case "AIC":									return "aic";
					case "BIC":									return "bic";
					case "EB-global":							return "ebGlobal";
					case "EB-local":							return "ebLocal";
					case "g-prior":								return "gPrior";
					case "hyper-g":								return "hyperG";
					case "hyper-g-laplace":						return "hyperGLaplace";
					case "hyper-g-n":							return "hyperGN";
					case "JZS":									return "jzs";
				}
			}
		}

		ChangeRename { from: "alpha";							to:	"gPriorAlpha"				}
		ChangeRename { from: "rScale";							to:	"jzsRScale"				}

		ChangeJS
		{
			name:		"modelPrior"
			jsFunction:	function(options)
			{
				switch(options["modelPrior"])
				{
					case "beta.binomial":				return "betaBinomial";
					case "Wilson":						return "wilson";
					case "Castillo":					return "castillo";
					case "Bernoulli":					return "bernoulli";
					return options["modelPrior"]
				}
			}
		}

		ChangeJS
		{
			name:		"samplingMethod"
			jsFunction:	function(options)
			{
				switch(options["samplingMethod"])
				{
					case "BAS":					return "bas";
					case "MCMC":				return "mcmc";
				}
			}
		}

		ChangeRename { from: "iterationsMCMC";								to:	"samples"									}
		ChangeRename { from: "nSimForCRI";									to:	"numericalAccuracy"							}
		

	}
}
