#' Simple pairwiseImageDistanceMatrix function for images
#'
#' Output contains the NImages x NImages matrix of
#' c('PearsonCorrelation','Mattes') or any Image Metric values available in
#' iMath.  Similarity is computed after an affine registration is
#' performed.  You can also cluster the images via the dissimilarity
#' measurement, i.e. the negated similarity metric.  So, the estimated
#' dissimilarity is returned in the matrix.
#'
#'
#' @param dim imageDimension
#' @param myFileList dd<-'MICCAI-2013-SATA-Challenge-Data/CAP/training-images/'
#' myFileList<-list.files(path=dd, pattern = glob2rx('*nii.gz'),full.names =
#' T,recursive = T)
#' @param metrictype similarity function
#' @param nclusters integer controlling max number of clusters to search over
#' @return raw dissimilarity matrix is output, symmetrized matrix and
#' clustering (optional) in a list
#' @author Avants BB
#' @examples
#'
#' \dontrun{
#'   # dsimdata<-pairwiseImageDistanceMatrix( 3, imagefilelist, nclusters = 5 )
#' }
#'
#' @export pairwiseImageDistanceMatrix
pairwiseImageDistanceMatrix <- function(dim,
  myFileList, metrictype = "PearsonCorrelation",
  nclusters = NA) {
  fnl <- length(myFileList)
  mymat <- matrix(rep(NA, fnl * fnl), nrow = fnl, ncol = fnl)
  tct <- 0
  for (ct in 1:fnl) {
    for (ct2 in 1:fnl) {
      if (ct != ct2) {
        i1 <- antsImageRead(myFileList[ct], dim)
        i2 <- antsImageRead(myFileList[ct2], dim)
        toutfn <- paste(tempdir(), "Z", sep = "/")
        mytx <- antsRegistration(fixed = i1, moving = i2, typeofTransform = c("AffineFast"),
          outprefix = toutfn)
        mywarpedimage <- antsApplyTransforms(fixed = i1, moving = i2, transformlist = mytx$fwdtransforms)
        # broken !!  metric <- capture.output(imageMath(dim, 'j', metrictype, i1,
        # mywarpedimage))[1]
        wh <- (mywarpedimage > 0 & i1 > 0)
        if (metrictype == "PearsonCorrelation") {
          metric <- abs(cor.test(i1[wh], mywarpedimage[wh])$est)
        } else {
          metric <- mean(abs(i1[wh] - mywarpedimage[wh]))
        }
        mymat[ct, ct2] <- (as.numeric(metric))
        tct <- tct + 1
        print(paste(100 * tct/(fnl * fnl), "%"))
        print(mymat)
      }
    }
  }
  if (metrictype == "PearsonCorrelation") {
    mymat <- mymat * (-1)  # make a dissimilarity matrix
  }
  mymat <- mymat - min(mymat, na.rm = T)  # make min zero
  symat <- (mymat + t(mymat)) * 0.5  # make symmetric
  if ( !is.na(nclusters) &  usePkg("cluster") ) {
    clusters <- rep(NA, fnl)
    clusterrep <- rep(NA, nclusters)
    pamx <- cluster::pam(symat, nclusters)
    clusters <- summary(pamx)$clustering
    for (nc in 1:nclusters) {
      wc <- c(1:fnl)[clusters == nc]
      if (length(dim(symat[, wc])) > 0) {
        means <- apply(symat[, wc], MARGIN = 2, mean, na.rm = T)
        clusterrep[nc] <- wc[which.min(means)]
      } else clusterrep[nc] <- which(clusters == nc)
    }
    return(list(rawMatrix = mymat, symmMatrix = symat, clusters = clusters, representatives = myFileList[clusterrep]))
  }
  return(list(rawMatrix = mymat, symmMatrix = symat))
}
