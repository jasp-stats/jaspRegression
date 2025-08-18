library("jaspTools")
setupJaspTools()

options <- analysisOptions('~/Documents/projects/jasp/jaspRegression/dan_testing/iris.jasp')
runAnalysis(dataset='~/Documents/projects/jasp/jaspRegression/dan_testing/iris.csv', options =  options)
