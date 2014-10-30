timeseries2matrix <- function(img, mask) {
  labs <- sort(unique(mask[mask > 0.001]))
  if (length(labs) == 1) 
    logmask <- (mask == 1) else logmask <- (mask > 0)
  mat <- img[logmask]
  dim(mat) <- c(sum(logmask), dim(img)[length(dim(img))])
  mat <- t(mat)
  if (length(labs) == 1) 
    return(mat)
  maskvec <- mask[logmask]
  mmat <- matrix(apply(mat[, maskvec == labs[1]], FUN = mean, MARGIN = 1), ncol = 1)
  for (i in 2:length(labs)) {
    newmat <- matrix(apply(mat[, maskvec == labs[i]], FUN = mean, MARGIN = 1), 
      ncol = 1)
    mmat <- cbind(mmat, newmat)
  }
  colnames(mmat) <- paste("L", labs)
  return(mmat)
} 
