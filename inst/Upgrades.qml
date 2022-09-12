
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
                switch(options["naAction"])
				{
					case "excludePairwise":		return "pairwise";
					case "excludeListwise":		return "listwise";
                    default:					return options["naAction"];
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

    Upgrade
    {
        functionName:		"CorrelationBayesian"
        fromVersion:		"0.16.3"
        toVersion:			"0.16.4"

        // MAIN SECTION - Additional options --------------
        ChangeRename { from: "displayPairwise";						to: "pairwiseDisplay"						}
        ChangeRename { from: "reportBayesFactors";					to: "bayesFactorReport"						}
        ChangeRename { from: "flagSupported";						to: "supportCorrelationFlagged"				}
        ChangeRename { from: "reportN";								to: "sampleSize"                            }
        ChangeRename { from: "ciValue";								to: "ciLevel"                               }

        ChangeJS
        {
            name:		"alternative"
            jsFunction:	function(options)
            {
                switch(options["alternative"])
                {
                    case "two.sided":				return "twoSided"	;
                    default:                        return options["alternative"];
                }
            }
        }

        // Main Plots
        ChangeRename { from: "plotMatrix";								to: "matrixPlot"							}
        ChangeRename { from: "plotMatrixDensities";						to: "matrixPlotDensity"						}
        ChangeRename { from: "plotMatrixPosteriors";					to: "matrixPlotPosterior"					}

        // Main section - Prior
        ChangeRename { from: "kappa";									to: "priorWidth"							}

        // PAIRS SECTION --------------
        ChangeRename { from: "pairs";									to: "variablePairs" 						}


        // Pairs Plots - Scatter plots
        ChangeRename { from: "plotScatter";								to: "scatterPlot"							}
        ChangeRename { from: "plotScatterAddInfo";						to: "scatterPlotAddInfo"					}

        // Pairs Plots - Prior posterior plots
        ChangeRename { from: "plotPriorPosterior";						to: "priorPosteriorPlot"					}
        ChangeRename { from: "plotPriorPosteriorAddEstimationInfo";		to: "priorPosteriorPlotAddEstimationInfo"	}
        ChangeRename { from: "plotPriorPosteriorAddTestingInfo";		to: "priorPosteriorPlotAddTestingInfo"		}

        // Pairs Plots - Robustness plots
        ChangeRename { from: "plotBfRobustness";						to: "bfRobustnessPlot"						}
        ChangeRename { from: "plotBfRobustnessAddInfo";					to: "bfRobustnessPlotAddInfo"				}

        // Pairs Plots - Sequential plots
        ChangeRename { from: "plotBfSequential";						to: "bfSequentialPlot"						}
        ChangeRename { from: "plotBfSequentialAddInfo";					to: "bfSequentialPlotAddInfo"				}


        // OPTIONS SECTION --------------
        ChangeRename { from: "missingValues";							to: "naAction"								}

        ChangeJS
        {
            name:		"naAction"
            jsFunction:	function(options)
            {
                switch(options["naAction"])
                {
                    case "excludePairwise":		return "pairwise";
                    case "excludeListwise":		return "listwise";
                    default:					return options["naAction"];
                }
            }
        }
    }


	Upgrade
	{
		functionName:		"GeneralizedLinearModel"
		fromVersion:		"0.16.3"
		toVersion:			"0.16.4"

		ChangeRename { from: "gofDeviance";					to: "devianceGoodnessOfFit"							}
		ChangeRename { from: "gofPearson";					to: "pearsonGoodnessOfFit"							}
		ChangeRename { from: "coefEstimates";				to: "coefficientEstimate"							}
		ChangeRename { from: "coefCi";						to: "coefficientCi"									}
		ChangeRename { from: "coefCiInterval";				to: "coefficientCiLevel"							}
		ChangeRename { from: "outlierQuanTable";			to: "quantileResidualOutlierTable"					}
		ChangeRename { from: "outlierQuanTableTopN";		to: "quantileResidualOutlierTableTopN"				}
		ChangeRename { from: "outlierStdTable";				to: "standardizedResidualOutlierTable"				}
		ChangeRename { from: "outlierStdTableTopN";			to: "standardizedResidualOutlierTableTopN"			}
		ChangeRename { from: "outlierStuTable";				to: "studentizedResidualOutlierTable"				}
		ChangeRename { from: "outlierStuTableTopN";			to: "studentizedResidualOutlierTableTopN"			}
		ChangeRename { from: "covRatio";					to: "covarianceRatio"								}
		ChangeRename { from: "cooksD";					    to: "cooksDistance"								    }
		ChangeRename { from: "devResVsYPlot";				to: "devianceResidualVsFittedPlot"					}
		ChangeRename { from: "devResVsXPlot";				to: "devianceResidualVsPredictorPlot"				}
		ChangeRename { from: "devResQqPlot";				to: "devianceResidualQqPlot"						}
		ChangeRename { from: "prsResVsYPlot";				to: "pearsonResidualVsFittedPlot"					}
		ChangeRename { from: "prsResVsXPlot";				to: "pearsonResidualVsPredictorPlot"				}
		ChangeRename { from: "prsResQqPlot";				to: "pearsonResidualQqPlot"							}
		ChangeRename { from: "quanResVsYPlot";				to: "quantileResidualVsFittedPlot"					}
		ChangeRename { from: "quanResVsXPlot";				to: "quantileResidualVsPredictorPlot"				}
		ChangeRename { from: "quanResQqPlot";				to: "quantileResidualQqPlot"						}
		ChangeRename { from: "partialPlot";					to: "partialResidualPlot"							}
		ChangeRename { from: "zVsEtaPlot";					to: "workingResponseVsLinearPredictorPlot"			}

		ChangeJS
		{
			name:		"family"
			jsFunction:	function(options)
			{
				switch(options["family"])
				{
				case "Gamma":		            return "gamma";
				case "inverse.gaussian":		return "inverseGaussian";
				default:					    return options["family"];
				}
			}
		}
	}

	Upgrade
	{
		functionName:		"RegressionLogistic"
		fromVersion:		"0.16.3"
		toVersion:			"0.16.4"

		ChangeRename { from: "wlsWeights";					             to: "weights"							                    }
		ChangeRename { from: "includeIntercept";					     to: "interceptTerm"							            }
		ChangeRename { from: "factorDescriptivesOpt";					 to: "descriptive"							                }
		ChangeRename { from: "confusionMatrixOpt";					     to: "confusionMatrix"							            }
		ChangeRename { from: "coeffEstimates";					         to: "coefficientEstimate"							        }
		ChangeRename { from: "coeffEstimatesBootstrapping";				 to: "coefficientBootstrap"							        }
		ChangeRename { from: "coeffEstimatesBootstrappingReplicates";	 to: "coefficientBootstrapReplicates"						}
		ChangeRename { from: "stdCoeff";					             to: "coefficientStandardized"							    }
		ChangeRename { from: "oddsRatios";					             to: "oddsRatio"							                }
		ChangeRename { from: "coeffCI";					                 to: "coefficientCi"							            }
		ChangeRename { from: "coeffCIInterval";					         to: "coefficientCiLevel"							        }
		ChangeRename { from: "coeffCIOR";					             to: "coefficientCiAsOddsRatio"							    }
		ChangeRename { from: "robustSEOpt";					             to: "robustSe"							        }
		ChangeRename { from: "VovkSellkeMPR";					         to: "vovkSellke"							                }
		ChangeRename { from: "multicolli";					             to: "multicollinearity"							        }
		ChangeRename { from: "Accu";					                 to: "accuracy"							                    }
		ChangeRename { from: "AUC";					                     to: "auc"							                        }
		ChangeRename { from: "Sens";					                 to: "sensitivity"							                }
		ChangeRename { from: "Spec";					                 to: "specificity"							                }
		ChangeRename { from: "Prec";					                 to: "precision"							                }
		ChangeRename { from: "Fmsr";					                 to: "fMeasure"							                    }
		ChangeRename { from: "BrierScr";					             to: "brierScore"							                }
		ChangeRename { from: "Hmsr";					                 to: "hMeasure"							                    }
		ChangeRename { from: "casewiseDiagnostics";					     to: "residualCasewiseDiagnostic"							}
		ChangeRename { from: "casewiseDiagnosticsType";					 to: "residualCasewiseDiagnosticType"						}
		ChangeRename { from: "casewiseDiagnosticsResidualZ";			 to: "residualCasewiseDiagnosticZThreshold"					}
		ChangeRename { from: "casewiseDiagnosticsCooksDistance";		 to: "residualCasewiseDiagnosticCooksDistanceThreshold"		}
		ChangeRename { from: "estimatesPlotsOpt";					     to: "conditionalEstimatePlot"							    }
		ChangeRename { from: "estimatesPlotsCI";					     to: "conditionalEstimatePlotCi"							}
		ChangeRename { from: "showPoints";					             to: "conditionalEstimatePlotPoints"						}
		ChangeRename { from: "predictedPlotOpt";					     to: "residualVsFittedPlot"							        }
		ChangeRename { from: "predictorPlotsOpt";					     to: "residualVsPredictorPlot"							    }
		ChangeRename { from: "squaredPearsonPlotOpt";					 to: "squaredPearsonResidualVsFittedPlot"					}
		ChangeRename { from: "rocPlotOpt";					             to: "rocPlot"							                    }
		ChangeRename { from: "rocPlotStep";					             to: "rocPlotCutoffStep"							        }
		ChangeRename { from: "rocPlotAddCutoffLabels";					 to: "rocPlotCutoffLabel"							        }
		ChangeRename { from: "prPlotOpt";					             to: "precisionRecallPlot"							        }
		ChangeRename { from: "prPlotStep";					             to: "precisionRecallPlotCutoffStep"						}
		ChangeRename { from: "prPlotAddCutoffLabels";					 to: "precisionRecallPlotCutoffLabel"						}
	}
}
