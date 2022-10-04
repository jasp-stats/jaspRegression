# Compute Model
.reglogisticComputeModel <- function(jaspResults, dataset, options) {
  if(!is.null(jaspResults[["glmRes"]]))
    return(jaspResults[["glmRes"]]$object)
  type <- "binomial"
  if (type == "binomial") {
    # Logistic regression
    ff <- .createGlmFormula(options)
    nf <- .createNullFormula(options)
    # calculate null and full models
    nullMod <- glm(nf, family = "binomial", data = dataset)
    fullMod <- glm(ff, family = "binomial", data = dataset)
    glmRes  <- .glmStep(nullMod, fullMod, dataset, method = options$method)
  } else
    .quitAnalysis("GLM type not supported")
  jaspResults[["glmRes"]] <- createJaspState(glmRes)
  jaspResults[["glmRes"]]$dependOn(optionsFromObject = jaspResults[["modelSummary"]])
  return(glmRes)
}

.createGlmFormula <- function(options) {
  # this function outputs a formula name with base64 values as varnames
  f <- NULL

  dependent <- options$dependent
  if (dependent == "")
    f <- 0~1 # mock formula, always works

  modelTerms <- options$modelTerms
  interceptTerm <- options$interceptTerm
  if (length(modelTerms) == 0) {
    if (interceptTerm)
      f <- formula(paste(.v(dependent), "~ 1"))
    else
      f <- formula(paste(.v(dependent), "~ 0"))
  } else {
    if (interceptTerm)
      t <- character(0)
    else
      t <- "0"
    for (i in seq_along(modelTerms)) {
      term <- modelTerms[[i]][["components"]]
      if (length(term) == 1)
        t <- c(t, .v(term))
      else
        t <- c(t, paste(.v(unlist(term)), collapse = ":"))
    }
    f <- formula(paste(.v(dependent), "~", paste(t, collapse = "+")))
  }
  return(f)
}

.createNullFormula <- function(options) {
  # this function outputs a formula name with base64 values as varnames
  f <- NULL
  dependent <- options$dependent
  if (dependent == "")
    return(NULL)

  modelTerms <- options$modelTerms
  interceptTerm <- options$interceptTerm

  t <- character(0)
  for (i in seq_along(modelTerms)) {
    nui <- modelTerms[[i]][["isNuisance"]]
    if (!is.null(nui) && nui) {
      term <- modelTerms[[i]][["components"]]
      if (length(term) == 1)
        t <- c(t, .v(term))
      else
        t <- c(t, paste(.v(unlist(term)), collapse = ":"))
    }
  }
  if (!interceptTerm)
    t <- c(t, "0")
  else
    t <- c(t, "1")

  return(formula(paste(.v(dependent), "~", paste(t, collapse = "+"))))
}

.glmStep <- function(nullModel, fullModel, dataset, method = "enter") {
  # .glmStep function
  # -----------------
  # INPUT: calculated glm objects: nullModel, fullModel, and a dataset
  # nullModel and fullModel should have a data argument
  #
  # OUTPUT: List of glm objects, where the nullModel is the first model
  # (method = enter, forward, stepwise) or the fullModel (method = backward)
  # The last model is the final model that was converged on.

  # first, create temporary environment with dataset for stepAIC() calls
  tempenv <- new.env()
  datname <- as.character(as.list(getCall(fullModel))$data)
  assign(datname, dataset, envir = tempenv)

  null <- nullModel$formula
  full <- fullModel$formula

  assign("nf", null, envir = tempenv)
  assign("ff", full, envir = tempenv)

  if (method == "enter") {
    modlist <- vector("list", 2)
    modlist[[1]] <- nullModel
    modlist[[2]] <- fullModel
  } else if (method == "backward") {
    stepOut <- MASS::stepAIC(fullModel,
                             scope = list(upper=full, lower = null),
                             trace = 0,
                             direction = "backward",
                             keep = function(m, b) list(m))
    modlist <- as.list(stepOut$keep[,1:ncol(stepOut$keep)])
  } else {
    # translate method to stepAIC direction
    direct <- ifelse(method == "forward", method, "both")
    stepOut <- JASPstepAIC(nullModel, full, trace = 0,
                           direction = direct,
                           keep = function(m, b) list(m))
    modlist <- as.list(stepOut$keep[,1:ncol(stepOut$keep)])
  }

  return(modlist)
}

.confusionMatAddColInfo <- function(table, levs, type) {
  table$addColumnInfo(name = "pred0", title = paste0(levs[1]), type = type, overtitle = gettext("Predicted"))
  table$addColumnInfo(name = "pred1", title = paste0(levs[2]), type = type, overtitle = gettext("Predicted"))
}

# Helper functions for the above.
.lrtest <- function(glmModel1, glmModel2) {
  # likelihood ratio test for model against null model
  if (glmModel1[["df.residual"]] > glmModel2[["df.residual"]]) {
    superModel <- glmModel2
    subModel <- glmModel1
  } else {
    superModel <- glmModel1
    subModel <- glmModel2
  }

  chisq <- max(0, subModel[["deviance"]] - superModel[["deviance"]])
  df <- subModel[["df.residual"]] - superModel[["df.residual"]]

  if (chisq == 0 || df == 0)
    p <- NULL
  else
    p <- 1-pchisq(chisq, df)
  return(list(stat = chisq, df = df, pval = p))
}

.hasNuisance <- function(options) {
  return(any(sapply(options$modelTerms, function(x) x[["isNuisance"]])))
}

.mcFadden <- function(glmModel, nullModel) {
  # https://eml.berkeley.edu/reprints/mcfadden/zarembka.pdf
  rightSide <- deparse(glmModel[["formula"]][[3]])
  if (length(rightSide == 1) && rightSide %in% c("1", "0")) {
    # intercept-only model needs fix because of computer precision limits
    return(0)
  } else
    return(max(c(0,1-(glmModel[["deviance"]]/nullModel[["deviance"]]))))
}

.nagelkerke <- function(glmModel, nullModel) {
  # https://doi.org/10.1093/biomet/78.3.691
  rightSide <- deparse(glmModel[["formula"]][[3]])
  if (length(rightSide == 1) && rightSide %in% c("1", "0")) {
    # intercept-only model needs fix because of computer precision limits
    return(NULL)
  } else {
    l0 <- -0.5*nullModel[["deviance"]]
    lm <- as.numeric(logLik(glmModel))
    n  <- length(glmModel[["y"]])
    coxSnell <- 1 - exp(l0 - lm)^(2 / n)
    denom <- 1 - exp(l0)^(2 / n)
    return(max(c(0,coxSnell / denom)))
  }
}

.tjur <- function(glmModel) {
  # http://dx.doi.org/10.1198/tast.2009.08210
  ps <- predict(glmModel, type = "response")
  ys <- glmModel[["y"]]
  return(abs(mean(ps[as.logical(ys)])-mean(ps[!as.logical(ys)])))
}

.coxSnell <- function(glmModel, nullModel) {
  rightSide <- deparse(glmModel[["formula"]][[3]])
  if (length(rightSide == 1) && rightSide %in% c("1", "0")) {
    # intercept-only model needs fix because of computer precision limits
    return(NULL)
  } else {
    l0 <- -0.5*nullModel[["deviance"]]
    lm <- as.numeric(logLik(glmModel))
    n  <- length(glmModel[["y"]])
    coxSnell <- 1 - exp(l0 - lm)^(2 / n)
    return(max(c(0,coxSnell)))
  }
}

.bic <- function(glmModel) {
  return(log(length(glmModel[["y"]]))*length(coef(glmModel))+glmModel[["deviance"]])
}

.stdEst <- function(glmModel, type = "X") {
  # This function fits a new model with scaled variables and outputs the coeffs:
  # type = "X" : covariates scaled
  # type = "Y" : outcome scaled
  # type = "XY" : covariates and outcomes scaled
  # NB: factors (dummy-coded) will never be scaled.
  if (attr(glmModel[["terms"]], "intercept"))
    b <- summary(glmModel)[["coefficients"]][-1,1]
  else
    b <- summary(glmModel)[["coefficients"]][,1]

  factors <- names(glmModel[["xlevels"]])
  xmod <- glmModel[["model"]][!names(glmModel[["model"]]) %in% factors][,-1]
  xfac <- glmModel[["model"]][names(glmModel[["model"]]) %in% factors]
  ymod <- glmModel[["model"]][1]

  if (type == "X")
    stdDat <- cbind(ymod, scale(xmod), xfac)
  else if (type == "Y")
    stdDat <- cbind(scale(ymod), xmod, xfac)
  else if (type == "XY")
    stdDat <- cbind(scale(ymod), scale(xmod), xfac)

  names(stdDat) <- names(glmModel[["model"]])

  stdMod <- stats::glm(formula = glmModel[["formula"]], data = stdDat,
                       family = glmModel[["family"]])

  return(coef(stdMod))
}

.confusionMatrix <- function(glmModel, cutoff=0.5) {
  cMat <- list()
  pred <- predict(glmModel,type = "response")
  obs  <- glmModel$y
  h    <- hmeasure::HMeasure(obs, pred, threshold = cutoff)
  m    <- matrix(c(h[["metrics"]][["TN"]], h[["metrics"]][["FN"]],
                   h[["metrics"]][["FP"]], h[["metrics"]][["TP"]]), 2)
  dimnames(m) <- list("Observed" = c(0, 1), "Predicted" = c(0, 1))
  h[["metrics"]][["Brier"]] <- .brierScore(obs, pred)
  cMat[["matrix"]]  <- m
  cMat[["metrics"]] <- h[["metrics"]]
  return(cMat)
}

.brierScore <- function(obs, pred) {
  return(sum((pred - obs)^2) / length(pred))
}

.formatTerm <- function(term, glmModel) {
  # input: string of model term & glmObj
  vars <- names(glmModel[["model"]][-1])

  if (attr(glmModel[["terms"]], "intercept"))
    vars <- c(vars, gettext("(Intercept)"))

  # escape special regex characters
  vars <- gsub("(\\W)", "\\\\\\1", vars, perl=TRUE)

  # regex patterns
  pat1 <- paste0("\\:","(?=(",paste(vars, collapse = ")|("),"))")
  pat2 <- paste0("(?<=(",paste(vars, collapse = ")|("),"))")

  # split up string into components
  spl  <- strsplit(term, pat1, perl = TRUE)[[1]]
  spl2 <- lapply(spl, function(t) strsplit(t, pat2, perl = TRUE))

  # format and add back together
  col <- lapply(spl2, function(s) {
    if (length(unlist(s)) > 1) {
      varname <- .unv(unlist(s)[1])
      levname <- unlist(s)[2]
      return(paste0(varname, " (", levname, ")"))
    } else
      return(.unv(unlist(s)))
  })
  col2 <- paste(unlist(col), collapse = " * ")

  return(col2)
}

.predLevel <- function(glmModel) {
  predVar <- as.character(glmModel[["terms"]])[2]
  return(levels(glmModel[["data"]][[predVar]])[2])
}

.glmLogRegRibbon <- function(logRes, var, ciInt = 0.95) {
  # This function calculates the estimation & CI datapoints for logreg plot
  fac     <- FALSE
  factors <- names(logRes$xlevels)
  mf      <- model.frame(logRes)
  outcome <- logRes$terms[[2]]
  cDat    <- mf[,!(colnames(mf) %in% c(factors, outcome))]
  if (length(cDat) == nrow(mf)) {
    cm        <- mean(cDat)
    names(cm) <- colnames(mf)[!(colnames(mf) %in% c(factors, outcome))]
  } else
    cm <- colMeans(cDat)


  if (length(factors) == 0 || !(var %in% factors)) {
    # Variable of interest is continuous
    # create 101-length data frame of repeated colmeans
    newDat <- data.frame(matrix(rep(cm, 101), nrow=101, byrow=TRUE))
    colnames(newDat) <- names(cm)
    # add factors to this data
    if (length(factors) > 0) {
      for (f in factors) {
        col <- factor(rep(logRes[["xlevels"]][[f]][1], 101),
                      levels = logRes[["xlevels"]][[f]])
        newDat <- data.frame(newDat, col)
        colnames(newDat)[ncol(newDat)] <- f
      }
    }
    # then change the variable of interest to a nice range
    vd <- pretty(mf[[var]])
    xs <- seq(min(vd), max(vd), length.out = 101)
    newDat[[var]] <- xs
  } else {
    # variable is factor
    fac <- TRUE
    levs <- paste0(.unv(var), logRes[["xlevels"]][[var]])
    nlevs <- length(levs)

    # create a new data frame of nlevs length
    newDat <- data.frame(matrix(rep(cm, nlevs), nrow=nlevs, byrow=TRUE))
    colnames(newDat) <- names(cm)
    # add factors to this data
    if (length(factors) > 0) {
      for (f in factors) {
        col <- factor(rep(logRes[["xlevels"]][[f]][1], nlevs),
                      levels = logRes[["xlevels"]][[f]])
        newDat <- data.frame(newDat, col)
        colnames(newDat)[ncol(newDat)] <- f
      }
    }
    # then change the factor of interest to a nice range
    xs <- factor(logRes[["xlevels"]][[var]],
                 levels = logRes[["xlevels"]][[var]])
    newDat[[var]] <- xs
  }

  pred <- predict(logRes, newdata = newDat, type = "link", se.fit = TRUE)
  ys   <- .invLogit(pred$fit)
  critValue <- qnorm(1 - (1 - ciInt) / 2)
  lo <- .invLogit(pred$fit - critValue * pred$se.fit)
  hi <- .invLogit(pred$fit + critValue * pred$se.fit)
  outFrame <- data.frame(x = xs, y = ys, lo = lo, hi = hi)

  attr(outFrame, "factor") <- fac

  return(outFrame)
}

.invLogit <- function(x) {
  return(1/(1 + exp(-x)))
}

.glmRobustSE <- function(glmModel) {
  # Robust SE courtesy of Dan Gillen (UC Irvine)
  if (is.matrix(glmModel[["x"]]))
    xmat <- glmModel[["x"]]
  else {
    mf   <- model.frame(glmModel)
    xmat <- model.matrix(terms(glmModel), mf)
  }

  umat      <- residuals(glmModel, "working") * glmModel[["weights"]] * xmat
  modelv    <- summary(glmModel)[["cov.unscaled"]]
  robustCov <- modelv%*%(t(umat)%*%umat)%*%modelv
  dimnames(robustCov) <- dimnames(vcov(glmModel))

  ##	Format the model output with p-values and CIs
  s <- summary(glmModel)
  robustSE <- sqrt(diag(robustCov))
  return(robustSE)
}

.casewiseDiagnosticsLogisticRegression <- function(dataset, model, options) {
  last <- length(model)

  # Values for all cases
  dependentAll        <- dataset[[.v(options$dependent)]]
  dependentAllNumeric <- rep(0, nrow(dataset))
  dependentAllNumeric[dependentAll == levels(dataset[[.v(options$dependent)]])[2]] <- 1
  predictedAll      <- predict(model[[last]], dataset, type = "response")
  predictedGroupAll <- rep(levels(dataset[[.v(options$dependent)]])[1], nrow(dataset))
  predictedGroupAll[predictedAll >= 0.5] <- levels(dataset[[.v(options$dependent)]])[2]
  residualAll  <- resid(model[[last]], type = "response")
  residualZAll <- resid(model[[last]], type = "pearson")
  cooksDAll    <- cooks.distance(model[[last]])

  # These will be the variables for the return object
  dependent      <- NA
  predicted      <- NA
  predictedGroup <- NA
  residual       <- NA
  residualZ      <- NA
  cooksD         <- NA


  if (options$residualCasewiseDiagnosticType == "residualZ")
    index <- which(abs(residualZAll) > options$residualCasewiseDiagnosticZThreshold)
  else if (options$residualCasewiseDiagnosticType == "cooksDistance")
    index <- which(abs(cooksDAll) > options$residualCasewiseDiagnosticCooksDistanceThreshold)
  else
    index <- seq_along(cooksDAll)

  if (length(index) == 0)
    index <- NA
  else {
    dependent      <- dependentAll[index]
    predicted      <- predictedAll[index]
    predictedGroup <- predictedGroupAll[index]
    residual       <- residualAll[index]
    residualZ      <- residualZAll[index]
    cooksD         <- cooksDAll[index]
  }
  casewiseDiag <- list(index          = unname(index),
                       dependent      = as.character(dependent),
                       predicted      = unname(predicted),
                       predictedGroup = as.character(predictedGroup),
                       residual       = unname(residual),
                       residualZ      = unname(residualZ),
                       cooksD         = unname(cooksD))
  return(casewiseDiag)
}

.reglogisticVovkSellke <- function(table, options) {
  table$addColumnInfo(name = "vsmpr",   title = gettextf("VS-MPR%s", "\u002A"), type = "number")
  message <- gettextf("Vovk-Sellke Maximum <em>p</em>-Ratio: Based on the <em>p</em>-value,
  the maximum possible odds in favor of H%1$s over H%2$s equals
  1/(-e <em>p</em> log(<em>p</em>)) for <em>p</em> %3$s .37
  (Sellke, Bayarri, & Berger, 2001).", "\u2081", "\u2080", "\u2264")
  table$addFootnote(message, symbol = "\u002A")
}

.reglogisticSetError <- function(res, table) {
  if(isTryError(res))
    table$setError(.extractErrorMessage(res))
}


## functions in package car 3.0-12
# Durbin-Watson
.durbinWatsonTest.lm <- function(model, max.lag=1, simulate=TRUE, reps=1000,
                                method=c("resample","normal"),
                                alternative=c("two.sided", "positive", "negative"), ...){
  method <- match.arg(method)
  alternative <- if (max.lag == 1) match.arg(alternative)
  else "two.sided"
  residuals <- residuals(model)
  if (any(is.na(residuals))) stop ('residuals include missing values')
  n <- length(residuals)
  r <- dw <-rep(0, max.lag)
  den <- sum(residuals^2)
  for (lag in 1:max.lag){
    dw[lag] <- (sum((residuals[(lag+1):n] - residuals[1:(n-lag)])^2))/den
    r[lag] <- (sum(residuals[(lag+1):n]*residuals[1:(n-lag)]))/den
  }
  if (!simulate){
    result <- list(r=r, dw=dw)
    class(result) <- "durbinWatsonTest"
    result
  }
  else {
    S <- summary(model)$sigma
    X <- model.matrix(model)
    mu <- fitted.values(model)
    Y <- if (method == "resample")
      matrix(sample(residuals, n*reps, replace=TRUE), n, reps) + matrix(mu, n, reps)
    else matrix(rnorm(n*reps, 0, S), n, reps) + matrix(mu, n, reps)
    E <- residuals(lm(Y ~ X - 1))
    DW <- apply(E, 2, .durbinWatsonTest.default, max.lag=max.lag)
    if (max.lag == 1) DW <- rbind(DW)
    p <- rep(0, max.lag)
    if (alternative == 'two.sided'){
      for (lag in 1:max.lag) {
        p[lag] <- (sum(dw[lag] < DW[lag,]))/reps
        p[lag] <- 2*(min(p[lag], 1 - p[lag]))
      }
    }
    else if (alternative == 'positive'){
      for (lag in 1:max.lag) {
        p[lag] <- (sum(dw[lag] > DW[lag,]))/reps
      }
    }
    else {
      for (lag in 1:max.lag) {
        p[lag] <- (sum(dw[lag] < DW[lag,]))/reps
      }
    }
    result <- list(r=r, dw=dw, p=p, alternative=alternative)
    class(result)<-"durbinWatsonTest"
    result
  }
}

.durbinWatsonTest.default <- function(model, max.lag=1, ...){
  # in this case, "model" is the residual vectors
  if ((!is.vector(model)) || (!is.numeric(model)) ) stop("requires vector of residuals")
  if (any(is.na(model))) stop ('residuals include missing values')
  n <-  length(model)
  dw <- rep(0, max.lag)
  den <- sum(model^2)
  for (lag in 1:max.lag){
    dw[lag] <- (sum((model[(lag+1):n] - model[1:(n-lag)])^2))/den
  }
  dw
}

# multicollineary statistics, also from package car 3.0-12
.vif.default <- function(mod, ...) {
  if (any(is.na(coef(mod))))
    stop ("there are aliased coefficients in the model")
  v <- vcov(mod)
  assign <- attr(model.matrix(mod), "assign")
  if (names(coefficients(mod)[1]) == "(Intercept)") {
    v <- v[-1, -1]
    assign <- assign[-1]
  }
  else warning("No intercept: vifs may not be sensible.")
  terms <- labels(terms(mod))
  n.terms <- length(terms)
  if (n.terms < 2) stop("model contains fewer than 2 terms")
  R <- cov2cor(v)
  detR <- det(R)
  result <- matrix(0, n.terms, 3)
  rownames(result) <- terms
  colnames(result) <- c("GVIF", "Df", "GVIF^(1/(2*Df))")
  for (term in 1:n.terms) {
    subs <- which(assign == term)
    result[term, 1] <- det(as.matrix(R[subs, subs])) *
      det(as.matrix(R[-subs, -subs])) / detR
    result[term, 2] <- length(subs)
  }
  if (all(result[, 2] == 1)) result <- result[, 1]
  else result[, 3] <- result[, 1]^(1/(2 * result[, 2]))
  result
}
