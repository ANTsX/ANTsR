#' Simple rapidlyInspectImageData function.
#'
#' rapidlyInspectImageData collects basic statistics over a dataset and returns them
#' in a dataframe.
#'
#' @param myfiles input list of filenames
#' @return matrix is output
#' @author Avants BB
#' @examples
#'
#' \dontrun{
#'  fnl <- c( getANTsRData("r16"),
#'  getANTsRData("r27"),
#'  getANTsRData("r62"),
#'  getANTsRData("r64"),
#'  getANTsRData("r85") )
#' mm<-rapidlyInspectImageData( fnl )
#' }
#'
#' @export rapidlyInspectImageData
rapidlyInspectImageData <- function( myfiles ) {
  if (nargs() == 0) {
    print(args(rapidlyInspectImageData))
    return(1)
  }
  if ( !usePkg("moments") ) { print("Need moments package"); return(NULL) }
  measnames <- c("mean1", "mean2", "sd1", "sd2", "kurt1", "kurt2")
  nmeas <- length(measnames)
  nfn <- length(myfiles)
  mymat <- matrix(rep(0, nfn * nmeas), nrow = nfn)
  colnames(mymat) <- measnames
  for (i in 1:nfn) {
    img <- antsImageRead(myfiles[i])
    if ( img@components > 1 ) img = splitChannels( img ) %>% antsAverageImages(verbose=FALSE)
    thresh <- mean(img[img > min(img)])
    mymat[i, 1] <- thresh
    mymat[i, 2] <- mean(img[img > thresh])
    mymat[i, 3] <- sd(img[img > min(img)])
    mymat[i, 4] <- sd(img[img > thresh])
    mymat[i, 5] <- moments::kurtosis(img[img > min(img)])
    mymat[i, 6] <- moments::kurtosis(img[img > thresh])
  }
  rownames(mymat) <- myfiles
  return(mymat)
}
