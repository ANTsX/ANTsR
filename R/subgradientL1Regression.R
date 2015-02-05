#' Simple subgradientL1Regression function.
#'
#' SubgradientL1Regression solves y approx x beta
#'
#' @param y outcome variable
#' @param x predictor matrix
#' @param s gradient descent parameter
#' @param percentvals percent of values to use each iteration
#' @param nits number of iterations
#' @param betas initial guess at solution
#' @param sparval sparseness
#' @return output has a list of summary items
#' @author Avants BB
#' @examples
#'
#' mat<-replicate(1000, rnorm(200))
#' y<-rnorm(200)
#' wmat<-subgradientL1Regression( y, mat, percentvals=0.05 )
#' print( wmat$resultcorr )
#'
#' @export subgradientL1Regression
subgradientL1Regression <- function(y, x, s = 0.01,
  percentvals = 0.1, nits = 100,
  betas = NA, sparval = NA) {
  if (nargs() == 0) {
    print("Usage:  betas<-subgradientL1Regression( y ,  x) ")
    print("Needs to be checked more carefully")
    return(1)
  }
  if (percentvals > 1 | percentvals < 0)
    percentvals <- 1
  if (is.na(betas))
    betas <- rep(0, ncol(x))
  nvals <- round(percentvals * ncol(x))
  if (nvals > ncol(x))
    nvals <- ncol(x)
  selectvals <- 1:nvals
  deltmag <- rep(0, nits)
  for (i in 1:nits) {
    mysamp <- sort(sample(1:ncol(x), size = nvals))
    delt <- (c(x[, mysamp] %*% betas[mysamp]) - y)
    deltmag[i] <- sqrt(sum(delt * delt))
    delt <- sign(delt)
    subgrad <- rep(0, ncol(x))
    subgrad[mysamp] <- t(x[, mysamp]) %*% as.matrix(delt)
    tk <- s/sqrt(sum(subgrad * subgrad))
    betas <- betas - tk * subgrad
    if (!is.na(sparval))
      betas <-.eanatsparsify(betas, sparval)
    resultcorr <- cor.test(y, x %*% betas)$est
  }
  resultcorr <- cor.test(y, x %*% betas)$est
  return(list(betas = betas, deltmag = deltmag, resultcorr = resultcorr))
}
