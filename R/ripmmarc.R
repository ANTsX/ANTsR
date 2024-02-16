
#' @title Scale space feature detector
#' @description Deploy a multiscale laplacian blob detector on an image.  The
#' function will return the blob descriptors as an image and data frame. NOTE:
#' you will get different features running on the raw image versus its negation.
#' @param image the input image
#' @param numberOfBlobsToExtract the estimated blob count
#' @param minScale the minimum amount of smoothing in scale space
#' @param maxScale the maximum amount of smoothing in scale space
#' @param stepsPerOctave the number of steps to take over scale space octaves
#' @param negate boolean to compute features from both image and its negation
#' @return list of antsImage and dataframe for blob descriptors
#' @author Avants BB
#' @examples
#'
#' # WIP: should explore effect of parameters further
#' blob1 = scaleSpaceFeatureDetection( ri( 1 ), 50 )
#' blob2 = scaleSpaceFeatureDetection( max( ri( 1 ) ) - ri( 1 ), 50 )
#'
#' @export scaleSpaceFeatureDetection
scaleSpaceFeatureDetection <- function( image, numberOfBlobsToExtract,
  minScale,
  maxScale,
  stepsPerOctave = 10,
  negate = FALSE
 )
{
  if ( missing( minScale ) ) minScale = min( antsGetSpacing(image) )*0.1
  if ( missing( maxScale ) ) maxScale = min( antsGetSpacing(image) )*64
  outimg = antsImageClone(image)*0.0
  temp = max(image) - image
  outblob <- ANTsRCore::blobAnalysis(
                     temp,
                     outimg,
                     numberOfBlobsToExtract,
                     minScale,
                     maxScale,
                     stepsPerOctave)
  if ( negate ) {
    outimgB = antsImageClone(image)*0.0
    outblobB <- ANTsRCore::blobAnalysis(
                       image,
                       outimgB,
                       numberOfBlobsToExtract,
                       minScale,
                       maxScale,
                       stepsPerOctave)
    outblob$blobImage = outblob$blobImage + outblobB$blobImage
    outblob$blobDescriptor =
      rbind( outblob$blobDescriptor, outblobB$blobDescriptor )
  }
  return( outblob )
}




#' @title Rotation Invariant Patch-based Multi-Modality Analysis aRChitecture
#' @description Patch-based and rotation invariant image decomposition.  This
#' is similar to patch-based dictionary learning in N-dimensions.  Warning:
#' there may be an overlow/underflow error in the C++ underlying this function
#' that appears to occur when insufficient \code{patchSamples} are selected.
#' @param img Image to decompose
#' @param mask Binary mask defining regions in which to decompose.
#' @param patchRadius Scalar radius defining the patch size.
#' @param patchSamples Scalar defining the number of random patches to sample.
#' @param patchVarEx Scalar defining the target variance explained.  If this is
#' greater than one, then it defines the number of eigenvectors.  Otherwise, it
#' defines the target variance explained.
#' @param meanCenter boolean whether we mean center the patches.
#' @param canonicalFrame pass in an existing canonicalFrame.
#' @param evecBasis pass in an existing eigenvector basis.
#' @param rotationInvariant boolean sets whether patches are rotationInvariant.
#' @param regressProjections boolean return reconstruction parameters.
#' @param verbose boolean sets verbosity.
#' @return list including the canonical frame, the matrix basis, the patches for
#' the full image, the projection coefficients for the full image, the
#' variance explained and a reconstructed image.
#' @author Kandel BM, Avants BB
#' @examples
#'
#' img <- antsImageRead( getANTsRData( "r16" ) )
#' msk <- getMask( img ) %>% iMath("ME",1)
#' lap = iMath( img, "Laplacian", 2 )
#' mskTestTrain = antsImageClone( msk )
#' mskTestTrain[1:128,1:256]=2
#' mskTestTrain = mskTestTrain * msk
#' pr = 2
#' nv = 15
#' ripped <- ripmmarc( img, thresholdImage(mskTestTrain,2,2), patchRadius=pr,
#'   patchSamples=5000, patchVarEx=nv, rotationInvariant = FALSE )
#' ipatches = ripped$imagePatchMat
#' ibasis   = scale( ripped$basisMat )
#' k = 5
#' kk = lm( ipatches[k,] ~ t( ibasis[1:10,] ) )
#' rimg = ripmmarcBasisImage( ripped$canonicalFrame, ipatches[k,] )
#' # plot( rimg, doCropping=FALSE )
#' bimg = ripmmarcBasisImage( ripped$canonicalFrame, ibasis[5,] )
#' # plot( bimg, doCropping=FALSE )
#'
#' rippedTest <- ripmmarc( img, thresholdImage(mskTestTrain,1,1), patchRadius=pr,
#'   evecBasis = ripped$basisMat, canonicalFrame = ripped$canonicalFrame,
#'   patchSamples=500, patchVarEx=nv, rotationInvariant = FALSE )
#' lapTrainVox = lap[ mskTestTrain == 2 ]
#' lapTestVox = lap[ mskTestTrain == 1 ]
#' mydftr = data.frame( lap=lapTrainVox, t1feats=ripped$evecCoeffs )
#' mydfte = data.frame( lap=lapTestVox, t1feats=rippedTest$evecCoeffs )
#' mdl = lm( lap ~ . , data = mydftr )
#' preds = predict( mdl, newdata = mydfte )
#' # cor.test( preds, mydfte$lap )
#'
#' @export
ripmmarc <- function(
  img,
  mask,
  patchRadius = 3,
  patchSamples = 1000,
  patchVarEx   = 0.95,
  meanCenter   = TRUE,
  canonicalFrame = NULL,
  evecBasis    = NULL,
  rotationInvariant = TRUE,
  regressProjections = TRUE,
  verbose = FALSE  ) {

  img = check_ants(img)
  mask = check_ants(mask)
  #  print("WARNING: WIP, this implementation of ripmmarc is not validated!!")
  inimg.float <- antsImageClone( img, "float" )
  mask.float <- antsImageClone( mask, "float" )
  outimg <- antsImageClone( inimg.float )
  if ( is.null(evecBasis ) ) evecBasis = matrix( nrow=0, ncol=0 )
  else if ( is.null(evecBasis ) & is.null( canonicalFrame ) )
    stop("The user must also pass in a canonical frame.")
  if ( is.null( canonicalFrame ) ) {
    canonicalFrame = makeImage( rep( 1, img@dimension ), 0 )
  }
  # FIXME not sure why transpose is needed below ....
  outstruct <- ANTsRCore::patchAnalysis(
                     inimg.float, mask.float, outimg, patchRadius,
                     patchSamples, patchVarEx,
                     meanCenter, canonicalFrame, t(evecBasis),
                     rotationInvariant, regressProjections, verbose)
  outstruct[[1]] = antsImageClone( outstruct[[1]], img@pixeltype )
  if ( regressProjections ) {
    mdl = lm( t( outstruct$imagePatchMat) ~ t( outstruct$basisMat  ) )
    bmdl = bigLMStats( mdl, lambda = 1.e-4, includeIntercept = T )
    outstruct$evecCoeffs = t( bmdl$beta )
    mdl = predict( mdl )
    outstruct$recon = makeImage( mask, mdl[ round( nrow( mdl ) / 2 ) + 1, ] )
  }
  invisible( gc() )
  return( outstruct )
}




#' @title Generate an antsImage from ripmmarc basis data
#' @description This function converts a vectorized image patch back into an
#' antsImage such that it may be displayed in its extrinsic dimensionality.
#' @param canonicalFrame canonical frame image generated by ripmmarc
#' @param patchBasisVector the basis vector to convert to image space
#' @param eps small value threshold to help generate mask from canonicalFrame
#' @return antsImage
#' @author Kandel BM, Avants BB
#' @examples
#'
#' img <- antsImageRead( getANTsRData( "rand" ) )
#' img = resampleImage( img, c( 32, 32 ) )
#' msk <- thresholdImage( img, quantile( img[ img > 0 ], 0.5 )[1], max( img ) )
#' ripped <- ripmmarc( img, msk, patchRadius=1, patchSamples=20, patchVarEx = 2 )
#' bimg = ripmmarcBasisImage( ripped$canonicalFrame, ripped$basisMat[1,] )
#'
#' @export
ripmmarcBasisImage <- function( canonicalFrame,
                                patchBasisVector, eps = 1.e-12)
{
  newimg = canonicalFrame * 0
  frameMask = thresholdImage( abs( canonicalFrame ), eps, Inf )
  if ( sum( frameMask ) != length( patchBasisVector ) )
    stop("Size of mask does not appear to match length of basis vector" )
  newimg[ frameMask == 1 ] = patchBasisVector
  return( newimg )
}






#' @title RIPMMARC population basis
#' @description Patch-based and rotation invariant image decomposition.  This
#' is similar to patch-based dictionary learning in N-dimensions.  This
#' implementation is more efficient for building a basis from image populations.
#' @param ilist list of antsImages from which to learn basis
#' @param mask Binary mask defining regions in which to decompose or a list
#' of masks corresponding to ilist.
#' @param patchRadius Scalar radius defining the patch size.
#' @param patchSamples Scalar defining the number of random patches to sample.
#' @param patchVarEx Scalar defining the target variance explained.  If this is
#' greater than one, then it defines the number of eigenvectors.  Otherwise, it
#' defines the target variance explained.
#' @param meanCenter boolean whether we mean center the patches.
#' @param seed seed to pass to \code{\link{randomMask}}.  This behavior is
#' different than setting the seed globally and running as \code{\link{randomMask}}
#' is called multiple times and that runs \code{\link{set.seed}}
#' @return list including the canonical frame, the matrix basis and its
#' eigenvalues
#' @author Kandel BM, Avants BB
#' @seealso \code{\link{ripmmarc}}
#' @examples
#'
#' pop = getANTsRData( "population" ) # list of example images
#' popmasks = list( )
#' for ( i in 1:length( pop ) )
#'   popmasks[[ i ]] = getMask( pop[[ i ]] )
#' set.seed(1234)
#' rp = ripmmarcPop( pop, popmasks, patchRadius=3,
#'   meanCenter = TRUE, patchSamples=1000 )
#' set.seed(1234)
#' rp2 = ripmmarcPop( pop, popmasks, patchRadius=3,
#'   meanCenter = TRUE, patchSamples=1000 )
#'   testthat::expect_equal(rp, rp2)
#' rp3 = ripmmarcPop( pop, popmasks, patchRadius=3,
#'   meanCenter = TRUE, patchSamples=1000, seed = 1234 )
#' testthat::expect_failure(testthat::expect_equal(rp, rp3))
#' \dontrun{
#' nv = 15
#' rippedTest <- ripmmarc( pop[[3]], popmasks[[3]], patchRadius = 3,
#'   evecBasis = rp$basisMat[1:nv,], patchVarEx = nv, meanCenter = TRUE,
#'   canonicalFrame = rp$canonicalFrame, regressProjections = TRUE )
#' mm = makeImage( popmasks[[3]], rippedTest$evecCoeffs[,1] )
#' }
#'
#' @export
ripmmarcPop <- function(
  ilist,
  mask,
  patchRadius=3,
  patchSamples=1000,
  patchVarEx=0.95,
  meanCenter=TRUE,
  seed)
  {
  maskIsList = class( mask ) == "list"
  if ( maskIsList ) {
    randMask = randomMask( mask[[1]], patchSamples, perLabel = TRUE,
                           seed = seed)
  } else randMask = randomMask( mask, patchSamples, perLabel = TRUE, seed = seed)
  ripped <- ripmmarc( ilist[[1]], randMask, patchRadius = patchRadius,
    patchSamples = patchSamples, patchVarEx = patchVarEx,
    rotationInvariant = FALSE )
#  spatialMatrix = imageDomainToSpatialMatrix( randMask,
#    thresholdImage( randMask, 1, Inf ) )
  # map all population images to neighborhood matrices
  idim = ilist[[1]]@dimension
  myrad = rep( patchRadius + 2, idim )
  ripmat = getNeighborhoodInMask( ilist[[ 1 ]], randMask, radius = myrad,
    boundary.condition = 'image' )
  for ( i in 2:length( ilist ) ) {
    if ( maskIsList ) {
      randMask = randomMask( mask[[i]], patchSamples, perLabel = TRUE,
                             seed = seed)
    }
    locmat = getNeighborhoodInMask( ilist[[ i ]], randMask, radius = myrad,
      boundary.condition = 'image' )
    ripmat = cbind( ripmat, locmat )
  }
  ripsvd = svd( antsrimpute( scale( ripmat, center=meanCenter, scale=FALSE ) ) )
  frameMask = thresholdImage(  abs( ripped$canonicalFrame ), 1.e-8, Inf )
  sel = as.numeric( frameMask )  > 0
  coreFrame = ripmmarcBasisImage( ripped$canonicalFrame, ripsvd$u[ sel, 2 ] )
  if ( meanCenter )
    coreFrame = ripmmarcBasisImage( ripped$canonicalFrame, ripsvd$u[ sel, 1 ] )
  list(
    canonicalFrame = coreFrame,
    basisMat = t( ripsvd$u[ sel,  ] ),
    basisEigenvalues = ripsvd$d
#    spatialMatrix = spatialMatrix
    )
  }
