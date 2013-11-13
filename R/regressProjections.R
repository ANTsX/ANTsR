regressProjections <- function(input.train, input.test, demog.train, demog.test, eigenvectors, mask, outcome, 
  covariates = "1", model.function = glm, which.eigenvectors = "all", ...) {
  input.train <- scale(as.matrix(input.train))
  input.test <- scale(as.matrix(input.test))
  input.train[is.nan(input.train)] <- 0
  input.test[is.nan(input.test)] <- 0
  projections.train <- matrix(rep(0, length(eigenvectors) * nrow(demog.train)), nrow = nrow(input.train), ncol = length(eigenvectors))
  projections.test <- matrix(rep(0, length(eigenvectors) * nrow(demog.test)), nrow = nrow(input.test), ncol = length(eigenvectors))
  vector.names <- rep(NA, length(eigenvectors))
  for (i in c(1:length(eigenvectors))) {
    vector.names[i] <- paste("eigvec", i, sep = "")
  }
  names(eigenvectors) <- vector.names
  colnames(projections.train) <- vector.names
  colnames(projections.test) <- vector.names
  for (i in c(1:length(eigenvectors))) {
    vector.masked <- eigenvectors[[i]][mask > 0]
    projections.train[, i] <- input.train %*% vector.masked
    projections.test[, i] <- input.test %*% vector.masked
  }
  demog.train <- cbind(demog.train, projections.train)
  demog.test <- cbind(demog.test, projections.test)
  # define formula
  base.formula <- paste(outcome, "~", covariates[1])
  if (length(covariates) > 1) {
    for (i in 2:length(covariates)) {
      base.formula <- paste(base.formula, "+", covariates[i])
    }
  }
  
  if (which.eigenvectors == "all") {
    my.formula <- base.formula
    for (i in 1:length(vector.names)) {
      my.formula <- paste(my.formula, "+", basename(vector.names[i]))
    }
    model.train <- model.function(formula = as.formula(my.formula), data = demog.train, ...)
    vectors.used <- vector.names
  } else if (which.eigenvectors == "optimal") {
    if (as.character(substitute(model.function)) != "glm") {
      warning("Warning: Attempting to use BIC-based model selection \n\t     on model that is not of type 'glm'.  This will fail \n\t     if your model function does not have an extractAIC method\n\t     and 'coefficients' attribute.")
    }
    formula.lo <- base.formula
    formula.hi <- formula.lo
    for (i in 1:length(vector.names)) {
      formula.hi <- paste(formula.hi, "+", basename(vector.names[i]))
    }
    formula.lo <- as.formula(formula.lo)
    formula.hi <- as.formula(formula.hi)
    model.initial <- model.function(formula = as.formula(formula.lo), data = demog.train, ...)
    model.optimal <- stepAIC(model.initial, scope = list(lower = as.formula(formula.lo), upper = as.formula(formula.hi)), 
      direction = c("both"), k = log(nrow(demog.train)), trace = 1)
    model.train <- model.function(formula = model.optimal$call, data = demog.train, ...)
    vectors.used <- rownames(summary(model.train)$coefficients)
    vectors.used <- vectors.used[grep("eigvec", vectors.used)]
  } else stop("which.eigenvectors must be either 'optimal' or 'all'.")
  
  # perform predictions
  outcome.real.train <- demog.train[, outcome]
  outcome.real.test <- demog.test[, outcome]
  if (class(outcome.real.train) == "numeric") {
    outcome.predicted.train <- predict(model.train, newdata = demog.train)
    outcome.predicted.test <- predict(model.train, newdata = demog.test)
    error.train <- mean(abs(outcome.predicted.train - outcome.real.train), na.rm = T)
    corcoeff.train <- cor.test(outcome.predicted.train, outcome.real.train)$estimate
    pvalue.train <- cor.test(outcome.predicted.train, outcome.real.train)$p.value
    error.test <- mean(abs(outcome.predicted.test - outcome.real.test), na.rm = T)
    corcoeff.test <- cor.test(outcome.predicted.test, outcome.real.test)$estimate
    pvalue.test <- cor.test(outcome.predicted.test, outcome.real.test)$p.value
    stats <- data.frame(error.train = error.train, corcoeff.train = corcoeff.train, pvalue.train = pvalue.train, 
      error.test = error.test, corcoeff.test = corcoeff.test, pvalue.test = pvalue.test)
  } else if (class(outcome.real.train) == "factor") {
    outcome.predicted.train.prob <- predict(model.train, newdata = demog.train, type = "response")
    outcome.predicted.train <- outcome.predicted.train.prob
    outcome.predicted.train[outcome.predicted.train.prob <= 0.5] <- levels(demog.train[, outcome])[1]
    outcome.predicted.train[outcome.predicted.train.prob > 0.5] <- levels(demog.train[, outcome])[2]
    outcome.predicted.train <- as.factor(outcome.predicted.train)
    outcome.predicted.test.prob <- predict(model.train, newdata = demog.test, type = "response")
    outcome.predicted.test <- outcome.predicted.test.prob
    outcome.predicted.test[outcome.predicted.test.prob <= 0.5] <- levels(demog.train[, outcome])[1]
    outcome.predicted.test[outcome.predicted.test.prob > 0.5] <- levels(demog.train[, outcome])[2]
    outcome.predicted.test <- as.factor(outcome.predicted.test)
    misclassification.rate.train <- length(outcome.predicted.train[outcome.predicted.train != outcome.real.train])/length(outcome.predicted.train)
    myglm.train <- glm(outcome.real.train ~ outcome.predicted.train, family = "binomial")
    pvalue.train <- data.frame(p.values = coefficients(summary(myglm.train))[, "Pr(>|z|)"])["outcome.predicted.train", 
      ]
    myglm.test <- glm(outcome.real.test ~ outcome.predicted.test, family = "binomial")
    pvalue.test <- data.frame(p.values = coefficients(summary(myglm.test))[, "Pr(>|z|)"])["outcome.predicted.test", 
      ]
    misclassification.rate.test <- length(outcome.predicted.test[outcome.predicted.test != outcome.real.test])/length(outcome.predicted.test)
    stats <- data.frame(error.train = misclassification.rate.train, pvalue.train = pvalue.train, error.test = misclassification.rate.test, 
      pvalue.test = pvalue.test)
    # FIXME -- add ROC analysis.
  } else {
    warning("Predicted outcome is neither numeric nor factor--no stats output.")
    stats <- NULL
  }
  outcome.comparison <- data.frame(predicted = outcome.predicted.test, real = demog.test[, outcome])
  
  list(stats = stats, outcome.comparison = outcome.comparison, eigenvectors = eigenvectors[vectors.used])
} 
