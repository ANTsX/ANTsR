#' Simple inspectImageData3D function.
#'
#' InspectImageData3D collects basic statistics over a dataset and returns them
#' in a dataframe.
#'
#'
#' @param myfiles input list of filenames
#' @return matrix is output
#' @author Avants BB
#' @examples
#'
#' mm<-inspectImageData3D(
#'  fnl<-c(getANTsRData('r16'),
#'  getANTsRData('r27'),
#'  getANTsRData('r62'),
#'  getANTsRData('r64'),
#'  getANTsRData('r85') ) )
#' if ( !usePkg("DMwR") | ! usePkg('fpc') )
#'    { print("Need DMwR and fpc packages") } else {
#' pamres <- pamk(mm,1:4)
#' outlier.scores <- lofactor( mm, k=5)
#' outliers <- order(outlier.scores, decreasing=T)
#' }
#'
#' @export inspectImageData3D
inspectImageData3D <- function(myfiles) {
  if (nargs() == 0) {
    print(args(inspectImageData3D))
    return(1)
  }
  if ( !usePkg("fpc") ) { print("Need fpc package"); return(NULL) }
  if ( !usePkg("moments") ) { print("Need moments package"); return(NULL) }
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
