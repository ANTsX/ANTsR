#' Convert a matrix to a form that can be used to initialize sparse cca and
#' pca.
#'
#' InitializeEigenanatomy is a helper function to initialize sparseDecom and
#' sparseDecom2.
#'
#'
#' @param mat input matrix where rows provide initial vector values
#' @param nreps nrepetitions to use
#' @param mask mask if available
#' @return list is output
#' @author Avants BB
#' @examples
#'
#' mat<-t(replicate(3, rnorm(100)) )
#' for ( i in 1:nrow(mat) ) mat[i, abs(mat[i,]) < 1 ]<-0
#' initdf<-initializeEigenanatomy( mat )
#' dmat<-replicate(100, rnorm(20))
#' eanat<-sparseDecom( dmat, inmask=initdf$mask,
#'   sparseness=0, smooth=0,
#'   initializationList=initdf$initlist, cthresh=0,
#'   nvecs=length(initdf$initlist) )
#' initdf2<-initializeEigenanatomy( mat, nreps=2 )
#' eanat<-sparseDecom( dmat, inmask=initdf$mask,
#'   sparseness=0, smooth=0, z=-0.5,
#'   initializationList=initdf2$initlist, cthresh=0,
#'   nvecs=length(initdf2$initlist) )
#' # now a regression
#' eanatMatrix<-imageListToMatrix(  eanat$eigenanatomyimages, initdf$mask )
#' # "averages" loosely speaking anyway
#' myEigenanatomyRegionAverages<-dmat %*% t( eanatMatrix )
#' dependentvariable<-rnorm( nrow(dmat) )
#' summary(lm( dependentvariable ~ myEigenanatomyRegionAverages ))
#'
#' nvox<-1000
#' dmat<-replicate(nvox, rnorm(20))
#' dmat2<-replicate(30, rnorm(20))
#' mat<-t(replicate(3, rnorm(nvox)) )
#' for ( i in 1:nrow(mat) ) {
#'   mat[i,]<-eanatsparsify( mat[i,] , 0.5^(i+1)  )
#'   print(paste(sum(mat[i,]>0)/ncol(mat), 0.5^(i+1)  ))
#' }
#' initdf<-initializeEigenanatomy( mat )
#' eanat<-sparseDecom2( list(dmat,dmat2), inmask=c(initdf$mask,NA),
#'   sparseness=c( -0.1, -0.2 ), smooth=0,
#'   initializationList=initdf$initlist, cthresh=c(0,0),
#'   nvecs=length(initdf$initlist), priorWeight = 0.1 )
#'
#' @export initializeEigenanatomy
initializeEigenanatomy <- function(initmat, mask = NA, nreps = 1) {
  nclasses <- nrow(initmat)
  classlabels <- rownames(initmat)
  if (is.null(classlabels))
    classlabels <- paste("init", 1:nclasses, sep = "")
  initlist <- list()
  if (is.na(mask)) {
    maskmat <- initmat * 0
    maskmat[1, ] <- 1
    mask <- as.antsImage(maskmat)
  }
  eanatnames <- rep(as.character("A"), nclasses * nreps)
  ct <- 1
  for (i in 1:nclasses) {
    vecimg <- antsImageClone(mask)
    initf <- initmat[i, ]
    vecimg[mask == 1] <- initf  # eanatsparsify( initf , sparval[1] )
    for (nr in 1:nreps) {
      initlist <- lappend(initlist, vecimg)
      eanatnames[ct + nr - 1] <- toString(classlabels[i])
    }
    ct <- ct + nreps
  }

  return(list(initlist = initlist, mask = mask, enames = eanatnames))
}
