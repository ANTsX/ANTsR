inspectImageData3D <- function(myfiles) {
  if (nargs() == 0) {
    print(args(inspectImageData3D))
    return(1)
  }
  pckg <- try(require(moments))
  if (!pckg) {
    getPckg("moments")
  }
  library(moments)
  pckg <- try(require(extremevalues))
  if (!pckg) {
    getPckg("extremevalues")
  }
  library(extremevalues)
  measnames <- c("mean1", "mean2", "sd1", "sd2", "kurt1", "kurt2")
  nmeas <- length(measnames)
  idim <- 3
  nfn <- length(myfiles)
  mymat <- matrix(rep(0, nfn * nmeas), nrow = nfn)
  colnames(mymat) <- measnames
  for (i in 1:nfn) {
    img <- antsImageRead(myfiles[i], idim)
    thresh <- mean(img[img > min(img)])
    mymat[i, 1] <- thresh
    mymat[i, 2] <- mean(img[img > thresh])
    mymat[i, 3] <- sd(img[img > min(img)])
    mymat[i, 4] <- sd(img[img > thresh])
    mymat[i, 5] <- kurtosis(img[img > min(img)])
    mymat[i, 6] <- kurtosis(img[img > thresh])
  }
  rownames(mymat) <- myfiles
  return(mymat)
}
 
