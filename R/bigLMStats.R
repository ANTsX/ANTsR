#' Efficiently compute basic statistical inference from regressions with
#' multiple outcomes
#'
#' This function simplifies calculating p-values from linear models in which
#' there are many outcome variables, such as in voxel-wise regressions.  To
#' perform such an analysis in R, you can concatenate the outcome variables
#' column-wise into an n by p matrix \code{y}, where there are n subjects and p
#' outcomes (see \code{Examples}).  Calling \code{lm(y~x)} calculates the
#' coefficients, but statistical inference is not provided.  This function
#' provides basic statistical inference efficiently.
#'
#'
#' @param mylm Object of class \code{lm}.
#' @param lambda Value of ridge penalty for inverting ill-conditioned matrices.
#' @param includeIntercept Whether or not to include p-values for intercept
#' term in result.
#' @return A list containing objects: \item{fstat}{F-statistic of whole model
#' (one value per outcome).} \item{pval.model}{p-value of model (one value per
#' outcome).} \item{beta}{Values of coefficients (one value per predictor per
#' outcome).} \item{beta.std}{Standard error of coefficients.}
#' \item{beta.t}{T-statistic of coefficients.} \item{beta.pval}{p-value of
#' coefficients.}
#' @author Kandel BM.
#' @examples
#'
#'
#' nsub <- 100
#' set.seed(1500)
#' x <- 1:nsub
#' y <- matrix(c(x + rnorm(nsub), sin(x)), nrow=nsub)
#' x <- cbind(x, x^2)
#' y1 <- y[, 1]
#' y2 <- y[, 2]
#' lm1 <- lm(y1~x)
#' lm2 <- lm(y2~x)
#' mylm <- lm(y ~ x)
#'
#' myest <- bigLMStats(mylm)
#' print(paste("R beta estimates for first outcome is", summary(lm1)$coefficients[-1,1],
#'             "and for second outcome is", summary(lm2)$coefficients[-1,1]))
#' print(paste("and our estimate is", as.numeric(myest$beta[,1]), as.numeric(myest$beta[,2])))
#' print(paste("R std error estimate for first outcome is", summary(lm1)$coefficients[-1,2],
#'             "and for second outcome is", summary(lm2)$coefficients[-1,2],
#'             "and our estimate is", myest$beta.std[,1], myest$beta.std[,2]))
#' print(paste("R t value estimate for first outcome is", summary(lm1)$coefficients[-1,3],
#'             "and for second outcome is", summary(lm2)$coefficients[-1,3],
#'             "and our estimate is", myest$beta.t[,1], myest$beta.t[,2]))
#' print(paste("R pval for first outcome is", summary(lm1)$coefficients[-1,4],
#'             "and for second outcome is", summary(lm2)$coefficients[-1,4],
#'             "and our estimate is", myest$beta.pval[,1], myest$beta.pval[,2]))
#'
#' @export bigLMStats
bigLMStats <- function(mylm, lambda = 0, includeIntercept = F) {
  veccoef <- FALSE
  if (is.null(dim(mylm$coefficients)))
    veccoef <- TRUE
  if (!includeIntercept) {
    if (veccoef)
      beta <- mylm$coefficients[-1] else beta <- mylm$coefficients[-1, ]
  } else beta <- mylm$coefficients
  myresponse <- model.response(model.frame(mylm))
  X <- model.matrix(mylm)
  dfr <- dim(X)[2] - 1
  dfe <- dim(X)[1] - dfr - 1
  if (is.vector(myresponse)) {
    msm <- sum((t(t(mylm$fitted.values) - mean(myresponse)))^2)/dfr
    mse <- sum((mylm$residuals)^2)/dfe
    fstat <- msm/mse
  } else {
    msm <- colSums((t(t(mylm$fitted.values) - colMeans(myresponse)))^2)/dfr
    mse <- colSums((mylm$residuals)^2)/dfe
    fstat <- msm/mse
  }
  pval.model <- pf(fstat, dfr, dfe, lower.tail = F)
  XtXinv <- solve(t(X) %*% X + diag(ncol(X)) * lambda)
  if (!includeIntercept) {
    if (dim(X)[2] > 2) {
      mycoefs <- diag(XtXinv[2:dim(X)[2], 2:dim(X)[2]])
    } else {
      mycoefs <- XtXinv[2, 2]
    }
  } else mycoefs <- diag(XtXinv[1:dim(X)[2], 1:dim(X)[2]])
  if (is.vector(mylm$residuals)) {
    beta.std <- sqrt(sum((mylm$residuals)^2)/mylm$df.residual * mycoefs)
  } else {
    beta.std <- t(sqrt(as.vector(colSums((mylm$residuals)^2)/mylm$df.residual) %o%
      mycoefs))
  }
  if (!includeIntercept) {
    if (veccoef)
      beta.t <- mylm$coefficients[-1]/beta.std
    if (!veccoef)
      beta.t <- mylm$coefficients[-1, ]/beta.std
  } else beta.t <- mylm$coefficients/beta.std
  beta.pval <- 2 * pt(-abs(beta.t), df = mylm$df.residual)
  list(fstat = fstat, pval.model = pval.model, beta = beta, beta.std = beta.std,
    beta.t = beta.t, beta.pval = beta.pval)
}
