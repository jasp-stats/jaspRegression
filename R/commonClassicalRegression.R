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
  for(i in seq_along(options$modelTerms)) {
    tmp <- rapply(
      object  = options$modelTerms[[i]]["components"], 
      f       = function(varNames, dataset) .harmonizeVariableEncoding(varNames, dataset),
      how     = "replace",
      dataset = dataset
    )
    options$modelTerms[[i]]$components <- tmp[[1]]

    # term specification: the variable of each term definition (a constant offset has none)
    for (j in seq_along(options$modelTerms[[i]][["definitions"]]))
      if (!is.null(options$modelTerms[[i]][["definitions"]][[j]][["variable"]]))
        options$modelTerms[[i]][["definitions"]][[j]][["variable"]] <- .harmonizeVariableEncoding(options$modelTerms[[i]][["definitions"]][[j]][["variable"]], dataset)
  }
  options
}
