#' antsrMotionCalculation
#'
#' Correct time-series data for motion.
#'
#' @param img antsImage, usually ND where D=4.
#' @param fixed Fixed image to register all timepoints to.  If not provided, mean image is used.
#' @param mask mask for image (ND-1).  If not provided, estimated from data.
#' @param typeofTransform One of \code{"Affine"}, \code{"Rigid"},
#' '\code{"BOLDAffine"}, \code{"BOLDRigid"}, \code{"QuickRigid"}.
#' @param getMotionDescriptors computes dvars and framewise displacement.  May
#' take additional memory.
#' @param verbose enables verbose output.
#' @param reproducible if \code{TRUE}, will execute 
#' \code{Sys.setenv(ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS = 1)} before
#' running to attempt a more reproducible result.  See
#' \url{https://github.com/ANTsX/ANTs/wiki/antsRegistration-reproducibility-issues}
#' for discussion.
#' @return List containing:
#' \itemize{
#'  \item{moco_img}{ Motion corrected time-series image.}
#'  \item{moco_params}{ Data frame of translation parameters.}
#'  \item{moco_avg_img}{ Average motion-corrected image.}
#'  \item{moco_mask}{ Mask used to calculate framewise displacement.}
#'  \item{fd}{ Time-series mean and max displacements.}
#'  \item{dvars}{ DVARS, derivative of frame-wise intensity changes.}
#' }
#' 
#' @note For reproducible results, you should run
#' \code{Sys.setenv(ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS = 1)}, 
#' which is what the \code{reproducible = TRUE} flag will do.
#' See \url{https://github.com/ANTsX/ANTs/wiki/antsRegistration-reproducibility-issues}
#' and \url{https://github.com/ANTsX/ANTsR/issues/210#issuecomment-377511054}
#' for discussion
#' @author BB Avants, Benjamin M. Kandel, JT Duda, Jeffrey S. Phillips
#' @examples
#' set.seed(120)
#' simimg<-makeImage(rep(5,4), rnorm(5^4))
#' stopifnot(abs(mean(simimg) - 0.0427369860965759) < 1e-10)
#' res = antsrMotionCalculation( simimg, reproducible = TRUE)
#' check = abs(colMeans(res$fd) - c(MeanDisplacement = 5.16324474446418, 
#' MaxDisplacement = 5.16324474446417))
#' stopifnot(all(check < 1e-10))
#' @export antsrMotionCalculation
antsrMotionCalculation <- function(
  img,
  fixed,
  mask,
  typeofTransform = c( "Rigid", "QuickRigid", "BOLDRigid", "Affine",
                       "AffineFast", "BOLDAffine" ),
  getMotionDescriptors = TRUE,
  verbose = FALSE,
  reproducible = TRUE
  )
{
  if (reproducible) {
    itk_threads = Sys.getenv("ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS")
    on.exit({
      Sys.setenv(ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS = itk_threads)
    })
    Sys.setenv(ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS = 1)
  }
  typeofTransform = match.arg(typeofTransform)
  imgdim = length( dim( img ) )
  subdim = imgdim - 1
  ntimes = dim( img )[ imgdim ]
  if ( missing( fixed )  )
    {
    fixed <- getAverageOfTimeSeries( img )
    }
  if ( missing( mask ) ) {
    mask = getMask( fixed )
  }
  extractSubImage <- function( img, vin )
    {
    temp = ANTsRCore::extractSlice( img, vin, img@dimension )
    subdim = img@dimension - 1
    xxx=antsSetDirection( temp, antsGetDirection( img )[ 1:subdim, 1:subdim ] )
    return( temp )
    }
  # now loop over all time points and register to the fixed images
  # create array holder for deformed images
  warpedSlices = list()
  if ( verbose )
    progress <- txtProgressBar(min = 1, max = ntimes, style = 3)
  for ( i in 1:ntimes ) {
    localImg = extractSubImage( img, i )
    locreg = antsRegistration( fixed = fixed, moving = localImg,
      typeofTransform = typeofTransform )
    warpedSlices[[i]] = locreg$warpedmovout
    localtxp = readAntsrTransform( locreg$fwdtransforms, subdim )
    localtxp = getAntsrTransformParameters( localtxp )
    if ( i ==  1 ) {
      mocoparams = matrix( nrow=ntimes, ncol=length(localtxp) )
      if ( verbose ) print( localtxp )
      }
    mocoparams[i, ] = localtxp
    if ( verbose ) setTxtProgressBar(progress, i )
    gc()
    }
  if ( verbose ) close( progress )
  moco_img = mergeListToNDImage( img, warpedSlices )
  rm( warpedSlices )
  gc()
  meanout = getAverageOfTimeSeries( moco_img )
  if ( getMotionDescriptors ) {
    tempmat <- timeseries2matrix( img, mask)
    dvars <- computeDVARS( tempmat )
    rm( tempmat )
    # finally, get framewise displacement
    tsimg <- antsImageClone( img, "double" )
    mocostats <- .antsMotionCorrStats0( tsimg, mask, mocoparams )
    fd <- as.data.frame( mocostats$Displacements )
    names(fd) <- c( "MeanDisplacement", "MaxDisplacement" )
  } else {
    fd=NA
    dvars=NA
  }
  # now do a posthoc mapping of the motion parameters to roll pitch yaw
  # in the special case of rigid mapping in 3D
  if ( ( grep( "Rigid", typeofTransform ) == 1 ) & ( imgdim == 4 ) )
    {
    mocoparamsR = matrix( nrow=ntimes, ncol=6 )
    for ( i in 1:ntimes )
      mocoparamsR[i, ] = .affine2distance( mocoparams[i, ] )
    mocoparams = mocoparamsR
    }
  colnames( mocoparams ) = paste( 'MOCOparam', 1:ncol( mocoparams ), sep='' )
  return
    (
    list(
      moco_img     = moco_img,
      moco_params  = mocoparams,
      moco_avg_img = meanout,
      moco_mask    = mask,
      fd           = fd,
      dvars        = dvars
      )
    )
}


.affine2distance<-function(affVals) {

	affVals<-as.numeric(affVals)

	dx<-affVals[10]
	dy<-affVals[11]
	dz<-affVals[12]

	rotx<-asin(affVals[7])
	roty<-atan2(affVals[8]/cos(rotx),affVals[9]/cos(rotx))
	rotz<-atan2(affVals[4]/cos(rotx),affVals[1]/cos(rotx))

	rotx<-rotx*360/(2*pi)
	roty<-roty*360/(2*pi)
	rotz<-rotz*360/(2*pi)

	return(c(dx, dy, dz, rotx, roty, rotz))

}



# FIXME - read and write function for momo objects
