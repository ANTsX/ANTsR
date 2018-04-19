#' antsrMotionCalculation
#'
#' Correct time-series data for motion.
#'
#' @param img antsImage, usually ND where D=4.
#' @param fixed Fixed image to register all timepoints to.  If not provided, mean image is used.
#' @param mask mask for image (ND-1).  If not provided, estimated from data.
#' @param typeofTransform One of \code{"Affine"}, \code{"Rigid"},
#' '\code{"BOLDAffine"}, \code{"BOLDRigid"}, \code{"QuickRigid"}.
#' @param verbose enables verbose output.
#' @return List containing:
#' \itemize{
#'  \item{moco_img}{ Motion corrected time-series image.}
#'  \item{moco_params}{ Data frame of translation parameters.}
#'  \item{moco_avg_img}{ Average motion-corrected image.}
#'  \item{moco_mask}{ Mask used to calculate framewise displacement.}
#'  \item{fd}{ Time-series mean and max displacements.}
#'  \item{dvars}{ DVARS, derivative of frame-wise intensity changes.}
#' }
#' @author BB Avants, Benjamin M. Kandel, JT Duda, Jeffrey S. Phillips
#' @examples
#' set.seed(120)
#' simimg<-makeImage(rep(5,4), rnorm(5^4))
#' antsrMotionCalculation( simimg )
#' @export antsrMotionCalculation
antsrMotionCalculation <- function(
  img,
  fixed = NA,
  mask  = NA,
  typeofTransform = "Rigid",
  verbose=FALSE
  )
{
  validTx = c( "Rigid", "QuickRigid", "BOLDRigid", "Affine",
     "AffineFast", "BOLDAffine" )
  if ( sum( typeofTransform  %in%  validTx ) == 0 )
    {
    print( "valid transform list:" )
    print( validTx )
    stop( paste( typeofTransform, "not in valid transform list." ) )
    }
  imgdim = length( dim( img ) )
  subdim = imgdim - 1
  ntimes = dim( img )[ imgdim ]
  if ( is.na( fixed )  )
    {
    fixed <- getAverageOfTimeSeries( img )
    }
  if ( is.na( mask ) ) mask = getMask( fixed )
  extractSubImage <- function( img, vin )
    {
    subdim = img@dimension-1
    subimg = makeImage(
      dim( img )[ 1:subdim ],
      voxval    = vin,
      spacing   = antsGetSpacing( img )[1:subdim],
      origin    = antsGetOrigin( img )[1:subdim],
      direction = antsGetDirection( img )[1:subdim,1:subdim]
      )
    }
  # now loop over all time points and register to the fixed images
  imgarr = as.array( img )
  # create array holder for deformed images
  fixarr = array( dim = c( dim( fixed ), ntimes ) )
  if ( verbose )
    progress <- txtProgressBar(min = 1, max = ntimes, style = 3)
  for ( i in 1:ntimes )
    {
    if ( imgdim == 2 ) localImg = extractSubImage( img, imgarr[ ,       i] )
    if ( imgdim == 3 ) localImg = extractSubImage( img, imgarr[ ,  ,    i] )
    if ( imgdim == 4 ) localImg = extractSubImage( img, imgarr[ ,  ,  , i] )
    locreg = antsRegistration( fixed = fixed, moving = localImg,
      typeofTransform = typeofTransform )
    if ( imgdim == 2 ) fixarr[ ,       i] = as.array( locreg$warpedmovout )
    if ( imgdim == 3 ) fixarr[ ,  ,    i] = as.array( locreg$warpedmovout )
    if ( imgdim == 4 ) fixarr[ ,  ,  , i] = as.array( locreg$warpedmovout )
#    localtxp = R.matlab::readMat( locreg$fwdtransforms )[[1]]
    localtxp = readAntsrTransform( locreg$fwdtransforms, subdim )
    localtxp = getAntsrTransformParameters( localtxp )
    if ( i ==  1 ) {
      mocoparams = matrix( nrow=ntimes, ncol=length(localtxp) )
      if ( verbose ) print( localtxp )
      }
    mocoparams[i, ] = localtxp
    if ( verbose ) setTxtProgressBar(progress, i )
    }
  if ( verbose ) close( progress )
  moco_img = as.antsImage( fixarr )
  antsSetSpacing( moco_img, c( antsGetSpacing(fixed),
    antsGetSpacing(img)[imgdim]) )
  antsSetOrigin( moco_img, c( antsGetOrigin(fixed),
    antsGetOrigin(img)[imgdim]) )
  mocodir = diag( imgdim )
  mocodir[ 1:subdim, 1:subdim ] = antsGetDirection( fixed )
  antsSetDirection( moco_img, mocodir )
  #  meanout <- apply( moco_img, c(1:(subdim)), mean, na.rm=T )
  meanout = getAverageOfTimeSeries( moco_img )
  tempmat <- timeseries2matrix( img, mask)
  dvars <- computeDVARS( tempmat )
  rm( tempmat )
  # finally, get framewise displacement
  tsimg <- antsImageClone( img, "double" )
  mocostats <- .antsMotionCorrStats0( tsimg, mask, mocoparams )
  fd <- as.data.frame( mocostats$Displacements )
  names(fd) <- c( "MeanDisplacement", "MaxDisplacement" )
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
