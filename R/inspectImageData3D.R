#' Simple inspectImageData3D function.
#'
#' InspectImageData3D collects basic statistics over a dataset and returns them
#' in a dataframe.
#'
#'
#' @param fn input list of filenames
#' @return matrix is output
#' @author Avants BB
#' @examples
#'
#' \dontrun{
#' mm<-inspectImageData3D( fnl<-c('r16slice.nii.gz','r64slice.nii.gz' ) )
#' usePkg('fpc')
#' pamres <- pamk(mm)
#' plot(pamres$pamobject)
#' outlier.scores <- DMwR::lofactor( mm, k=5)
#' outliers <- order(outlier.scores, decreasing=T)[1:5]
#' }
#'
#' @export inspectImageData3D
inspectImageData3D <- function(myfiles) {
  if (nargs() == 0) {
    print(args(inspectImageData3D))
    return(1)
  }
  # usePkg("moments")
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
