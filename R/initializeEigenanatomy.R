#' Convert a matrix to a form that can be used to initialize sparse cca and
#' pca.
#'
#' InitializeEigenanatomy is a helper function to initialize sparseDecom and
#' sparseDecom2.  Can be used to estimate sparseness parameters per eigenvector.
#' The user then only chooses nvecs and optional regularization parameters.
#'
#' @param initmat input matrix where rows provide initial vector values.
#' alternatively, this can be an antsImage which contains labeled regions.
#' @param mask mask if available
#' @param nreps nrepetitions to use
#' @param smoothing if using an initial label image, optionally smooth each roi
#' @return list is output
#' @author Avants BB
#' @examples
#'
#' mat<-t(replicate(3, rnorm(100)) )
#' initdf<-initializeEigenanatomy( mat ) # produces a mask
#' dmat<-replicate(100, rnorm(20)) # data matrix
#' svdv = t( svd( mat, nu=0, nv=10 )$v )
#' ilist = matrixToImages( svdv, initdf$mask )
#' eseg = eigSeg( initdf$mask, ilist,  TRUE  )
#' eanat<-sparseDecom( dmat, inmask=initdf$mask,
#'  sparseness=0, smooth=0,
#'  initializationList=ilist, cthresh=0,
#'  nvecs=length(ilist) )
#' initdf2<-initializeEigenanatomy( mat, nreps=2 )
#' eanat<-sparseDecom( dmat, inmask=initdf$mask,
#'   sparseness=0, smooth=0, z=-0.5,
#'   initializationList=initdf2$initlist, cthresh=0,
#'   nvecs=length(initdf2$initlist) )
#' # now a regression
#' eanatMatrix<-eanat$eigenanatomyimages
#' # 'averages' loosely speaking anyway
#' myEigenanatomyRegionAverages<-dmat %*% t( eanatMatrix )
#' dependentvariable<-rnorm( nrow(dmat) )
#' summary(lm( dependentvariable ~ myEigenanatomyRegionAverages ))
#'
#' nvox<-1000
#' dmat<-replicate(nvox, rnorm(20))
#' dmat2<-replicate(30, rnorm(20))
#' mat<-t(replicate(3, rnorm(nvox)) )
#' initdf<-initializeEigenanatomy( mat )
#' eanat<-sparseDecom2( list(dmat,dmat2), inmask=list(initdf$mask,NA),
#'   sparseness=c( -0.1, -0.2 ), smooth=0,
#'   initializationList=initdf$initlist, cthresh=c(0,0),
#'   nvecs=length(initdf$initlist), priorWeight = 0.1 )
#'
#' @export initializeEigenanatomy
initializeEigenanatomy <- function(initmat, mask = NULL, nreps = 1,
  smoothing = 0 ) {
  if ( class(initmat)[1] == 'antsImage' )
    {
    selectvec = initmat > 0
    if ( ! is.null( mask ) ) {
      mask = check_ants(mask)
      selectvec = mask > 0
    }
    initmatvec = initmat[ selectvec ]
    ulabs = sort( unique( initmatvec ) )
    ulabs = ulabs[ ulabs > 0 ]
    nvox = length( initmatvec )
    temp = matrix( nrow=length(ulabs) , ncol=nvox )
    rnmsx = paste(ulabs,sep='')
    for ( x in 1:length(ulabs) )
      {
      timg = thresholdImage( initmat, ulabs[x], ulabs[x] )
      if ( smoothing > 0 ) timg = smoothImage( timg, smoothing )
      temp[ x , ] = timg[ selectvec ]
      }
    initmat = temp
    rownames( initmat ) = rnmsx
    }
  nclasses <- nrow(initmat)
  classlabels <- rownames(initmat)
  if (is.null(classlabels))
    classlabels <- paste("init", 1:nclasses, sep = "")
  initlist <- list()
  if (is.null(mask)) {
    maskmat <- initmat * 0
    maskmat[1, ] <- 1
    mask <- as.antsImage(maskmat)
  }
  eanatnames <- rep(as.character("A"), nclasses * nreps)
  ct <- 1
  for (i in 1:nclasses) {
    vecimg <- antsImageClone(mask)
    initf <- initmat[i, ]
    vecimg[mask == 1] <- initf  #.eanatsparsify( initf , sparval[1] )
    for (nr in 1:nreps) {
      initlist[[ct]] <- vecimg
      eanatnames[ct + nr - 1] <- toString(classlabels[i])
      ct <- ct + 1
    }
  }
  return(list(initlist = initlist, mask = mask, enames = eanatnames))
}
