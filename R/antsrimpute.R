antsrimpute <- function(mydat, FUN = mean, ...) {
  if (is.null(dim(mydat))) {
    mydat2 <- mydat
    mydat2[is.na(mydat)] <- FUN(as.numeric(mydat), na.rm = T, ...)
    mydat2
  } else {
    mydat2 <- mydat
    for (x in 1:ncol(mydat)) mydat2[is.na(mydat[, x]), x] <- FUN(as.numeric(mydat[, 
      x]), na.rm = T, ...)
    mydat2
  }
} 
