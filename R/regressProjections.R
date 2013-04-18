regressProjections <- function(input.train, input.test, demog.train, demog.test, eigenvectors, mask, outcome, covariates = "1", method = "optimal" )
{
  input.train <- scale(as.matrix(input.train))
  input.test  <- scale(as.matrix(input.test))
  input.train[is.nan(input.train)] <- 0
  input.test[is.nan(input.test)]   <- 0
  projections.train <- matrix(rep(0, length(eigenvectors) * nrow(demog.train)), 
                               nrow = nrow(input.train), ncol = length(eigenvectors)) 
  projections.test <- matrix(rep(0, length(eigenvectors) * nrow(demog.test)), 
                              nrow = nrow(input.test), ncol = length(eigenvectors)) 
  vector.names <- rep(NA, length(eigenvectors))
  for (i in c(1:length(eigenvectors))) {
    vector.names[i] <- paste("eigvec", i, sep='')
  }
  names(eigenvectors) <- vector.names
  colnames(projections.train) <- vector.names 
  colnames(projections.test)  <- vector.names
  for (i in c(1:length(eigenvectors))) {
    vector.masked <- eigenvectors[[i]][mask > 0]
    projections.train[, i] <- input.train %*% vector.masked
    projections.test[, i]  <- input.test %*% vector.masked 
  }
  demog.train <- cbind(demog.train, projections.train)
  demog.test  <- cbind(demog.test, projections.test)
  # define formula
  base.formula <- paste(outcome, "~", covariates[1])
  if (length(covariates) > 1) {
    for (i in 2:length(covariates)) {
      base.formula <- paste(base.formula, "+", covariates[i])
    }
  }
  
  if (method == "all") {
    my.formula <- base.formula
    for (i in 1:length(vector.names)) {
      my.formula <- paste(my.formula, "+", basename(vector.names[i]))
    }
    lm.train <- lm(as.formula(my.formula), demog.train)
  }  else if(method == "optimal") {
    formula.lo <- base.formula
    formula.hi <- formula.lo
    for (i in 1:length(vector.names)) {
      formula.hi <- paste(formula.hi, "+", basename(vector.names[i]))
    }
    formula.lo <- as.formula(formula.lo)
    formula.hi <- as.formula(formula.hi)
    lm.initial <- lm( as.formula(formula.lo), demog.train )
    model.optimal <- stepAIC(lm.initial, 
                             scope=list(lower=as.formula(formula.lo), upper=as.formula(formula.hi)), 
                             direction=c("both"), k = log(nrow(demog.train)), trace=1)
    lm.train <- lm(model.optimal$call, demog.train)
  } else stop("Method must be either 'optimal' or 'all'.")
  vectors.used <- rownames(summary(lm.train)$coefficients)
  vectors.used <- vectors.used[grep("eigvec", vectors.used)]
  
  # perform predictions
  outcome.predicted.train <- predict(lm.train, newdata = demog.train)
  error.train             <- mean(abs(outcome.predicted.train - demog.train[, outcome] ), na.rm=T )
  corcoeff.train          <- cor.test(outcome.predicted.train, demog.train[, outcome ] )$estimate
  pvalue.train            <- cor.test(outcome.predicted.train, demog.train[, outcome ] )$p.value
  outcome.predicted.test <- predict(lm.train, newdata = demog.test)
  error.test             <- mean(abs(outcome.predicted.test - demog.test[, outcome] ), na.rm=T)
  corcoeff.test          <- cor.test(outcome.predicted.test, demog.test[, outcome ] )$estimate
  pvalue.test            <- cor.test(outcome.predicted.test, demog.test[, outcome ] )$p.value
  stats <- data.frame(error.train = error.train, corcoeff.train = corcoeff.train, 
                      pvalue.train = pvalue.train, 
		      error.test = error.test, corcoeff.test = corcoeff.test, 
		      pvalue.test = pvalue.test)
  outcome.comparison <- data.frame(predicted = outcome.predicted.test, 
                                   real = demog.test[, outcome ] )
  
  list(stats = stats, outcome.comparison = outcome.comparison, eigenvectors = eigenvectors[vectors.used] )
}

