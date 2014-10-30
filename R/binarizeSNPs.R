binarizeSNPs <- function(snps) {
  if (nargs() == 0) {
    print("Usage:  x_b<-binarizeSNPs( x ) ")
    return(1)
  }
  nrep <- 2
  binsnps <- (matrix(rep(NA, nrow(snps) * ncol(snps) * nrep), nrow = nrow(snps), 
    ncol = ncol(snps) * nrep))
  binsnpsdf <- data.frame(matrix(rep(NA, nrow(snps) * ncol(snps) * nrep), nrow = nrow(snps), 
    ncol = ncol(snps) * nrep))
  inds1 <- seq(1, (ncol(binsnps)), by = 2)
  inds2 <- inds1 + 1
  binsnps[, inds1] <- snps
  binsnps[, inds2] <- snps
  ww <- (binsnps[, inds1] == 2)
  binsnps[, inds1][ww] <- 0
  binsnps[, inds1][!ww] <- 1
  ww <- (binsnps[, inds2] == 1)
  binsnps[, inds2][ww] <- 0
  binsnps[, inds2][!ww] <- 1
  osnps <- data.frame(binsnps)
  names(osnps)[inds1] <- paste(names(snps), ".1", sep = "")
  names(osnps)[inds2] <- paste(names(snps), ".2", sep = "")
  return(osnps)
} 
