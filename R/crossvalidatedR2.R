#' @name crossvalidatedR2
#' @title Cross-Validated R^2 value
#' @description Computes an R^2 value for predicting an outcome measure using a k-fold cross-validation scheme.
#' @usage crossvalidatedR2(x, y, ngroups=5, covariates=NA, fast=F)
#' @param x Input predictor matrix.
#' @param y Target dependent variable.
#' @param ngroups Number of cross-validation folds to use or the fold labels themselves, equal to the length of y.  e.g. c(1,1,1,2,2,2...)
#' @param covariates Covariate predictors.
#' @param fast Use low-level \code{lm.fit} instead of \code{lm}.  Much faster, but less error checking.
#' @return Matrix of size \code{ngroups} by \code{ncol(x)}, which each row corresponding to one fold and the columns corresponding to the R2 values for each predictor.
#' @author Brian B Avants, Benjamin M. Kandel
#' @examples
#' set.seed(300)
#' ncol <- 30
#' nrow <- 20
#' covariate <- sin((1:nrow) * 2 * pi / nrow)
#' x <- matrix(rep(NA, nrow * ncol), nrow = nrow)
#' xsig <- seq(0, 1, length.out = nrow)
#' y <- xsig + covariate + rnorm(nrow, sd = 0.5)
#' for (i in 1:ncol) {
#'   x[, i] <- xsig + rnorm(nrow, sd = i / ncol)
#' }
#' r2 <- crossvalidatedR2(x, y, covariates = covariate)
#' @export crossvalidatedR2
crossvalidatedR2 <- function(x, y, ngroups = 5, covariates = NA, fast = F) {
  havecaret <- usePkg("caret")
  nvox <- ncol(x)
  if (length(ngroups) == 1) {
    if (havecaret) {
      groups <- caret::createFolds(y, k = ngroups, list = FALSE)
    } else {
      groups <- rep_len(c(1:ngroups), length(y))
    }
  } else {
    groups <- ngroups
  }
  ngroups <- length(unique(groups))
  R2v <- matrix(rep(0, ngroups * nvox), nrow = ngroups)
  if (!is.matrix(covariates)) {
    covariates <- as.matrix(covariates)
  }

  for (k in 1:ngroups) {
    selector <- groups != k
    if (!fast) {
      mydf <- data.frame(y[selector])
      if (!all(is.na(covariates))) {
        mydf <- data.frame(mydf, covariates[selector, ])
      }
      mylm1 <- lm(x[selector, ] ~ ., data = mydf)
    } else {
      y.sel <- cbind(rep(1, sum(selector)), y[selector])
      if (!all(is.na(covariates))) {
        y.sel <- cbind(y.sel, covariates[selector, ])
      }
      myfit <- lm.fit(x = y.sel, y = x[selector, ]) # do lm(x~y)-style fit
    }
    selector <- groups == k
    if (!fast) {
      mydf <- data.frame(y[selector])
      if (!all(is.na(covariates))) {
        mydf <- data.frame(mydf, covariates[selector, ])
      }
      predmat <- predict(mylm1, newdata = mydf)
    } else {
      y.sel <- cbind(rep(1, sum(selector)), y[selector])
      if (!all(is.na(covariates))) {
        y.sel <- cbind(y.sel, covariates[selector, ])
      }
      predmat <- y.sel %*% myfit$coefficients
    }
    realmat <- x[selector, ]
    sum1vec <- colSums((predmat - realmat)^2, na.rm = T)
    temp <- matrix(rep(colMeans(realmat, na.rm = T), nrow(realmat)),
      nrow = nrow(realmat),
      byrow = T
    )
    sum2vec <- colSums((temp - realmat)^2, na.rm = T)
    R2vec <- 100 * (1 - sum1vec / sum2vec)
    R2v[k, ] <- R2vec
  }
  R2v
}
