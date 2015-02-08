#' Split time series image into k distinct images
#'
#' Uses clustering methods to split a time series into similar subsets.
#'
#'
#' @param mat input time series matrix
#' @param krange k cluster range to explore
#' @param nsvddims eg 2
#' @param criterion for clustering see pamk
#' @return matrix is output
#' @author Avants BB
#' @examples
#'
#' \dontrun{
#'   if (!exists('fn') ) fn<-'PEDS029_20101110_pcasl_1.nii.gz'
#'    # high motion subject
#'   asl<-antsImageRead(fn,4)
#'   tr<-antsGetSpacing(asl)[4]
#'   aslmean<-getAverageOfTimeSeries( asl )
#'   aslmask<-getMask(aslmean,lowThresh=mean(aslmean),cleanup=TRUE)
#'   omat<-timeseries2matrix(asl, aslmask )
#'   clustasl<-clusterTimeSeries( omat, krange=4:10 )
#'   for ( ct in 1:max(clustasl$clusters) )
#'     {
#'     sel<-clustasl$clusters != ct
#'     img<-matrix2timeseries( asl, aslmask, omat[sel,] )
#'     perf <- aslPerfusion( img, interpolation='linear',
#'       dorobust=0.9, useDenoiser=4, skip=10, useBayesian=0,
#'       moreaccurate=0, verbose=F, mask=aslmask )
#'     perfp <- list( sequence="pcasl", m0=perf$m0 )
#'     cbf <- quantifyCBF( perf$perfusion, perf$mask, perfp )
#'     ofn<-paste('temp',ct,'.nii.gz',sep='')
#'     antsImageWrite( cbf$kmeancbf , ofn )
#'     ct<-ct+1
#'     }
#'   }
#'
#' @export clusterTimeSeries
clusterTimeSeries <- function(mat, krange = 2:10,
  nsvddims = NA, criterion = "asw") {
  if (nargs() == 0) {
    print(args(clusterTimeSeries))
    return(1)
  }
  if ( !usePkg("fpc") ) { print("Need fpc package"); return(NULL) }
  if (is.na(nsvddims))
    nsvddims <- (max(krange) * 2)
  # mat<-timeseries2matrix( img, mask )
  if (nsvddims > nrow(mat))
    nsvddims <- nrow(mat)/2
  matsvd <- svd(mat, nu = nsvddims, nv = 0)
  pk <- pamk(mat, krange = krange, criterion = criterion)
  clusters <- pk$pamobject$clustering
  tsimagelist <- list()
  for (i in 1:max(clusters)) {
    kmat <- mat[clusters != i, ]
    tsimagelist[[i]] <- kmat
    # matrix2timeseries( img, mask, kmat )
  }
  return(list(kimgs = tsimagelist, clusters = clusters))
}
