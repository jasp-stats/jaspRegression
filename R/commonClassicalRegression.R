## Given a vector of variable names with the old, type-free encoding, return the corresponding vector of names using the 
## new encoding (i.e., the encoding that includes the variable type information)
.harmonizeVariableEncoding <- function(varNames, dataset) {
  encodedDataNames <- colnames(dataset)
  decodedDataNames <- decodeColNames(encodedDataNames)
  decodedVarNames  <- decodeColNames(varNames)

  sapply(decodedVarNames,
    FUN = function(x) encodedDataNames[which(decodedDataNames %in% x)],
    USE.NAMES = FALSE)
}

## Apply the correct column encoding (including variable type information) to the variable names stored in
## 'options$modelTerms'
.encodeModelTerms <- function(options, dataset) {
  for(i in seq_along(options$modelTerms))
    options$modelTerms[[i]]$components <- .harmonizeVariableEncoding(options$modelTerms[[i]]$components, dataset)

  options
}

