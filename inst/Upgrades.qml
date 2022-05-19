
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
		functionName:	"Correlation"
		fromVersion:	"0.16.3"
		toVersion:		"0.16.4"

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
}
