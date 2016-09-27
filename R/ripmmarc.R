#' @name ripmmarc
#' @title Rotation Invariant Patch-based Multi-Modality Analysis aRChitecture
#' @description Patch-based and rotation invariant image decomposition.  This
#' is similar to patch-based dictionary learning in N-dimensions.
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
#' @param verbose boolean sets verbosity.
#' @return list including the canonical frame, the matrix basis, the patches for
#' the full image, the projection coefficients for the full image and the
#' variance explained.
#' @author Kandel BM, Avants BB
#' @examples
#' img <- antsImageRead( getANTsRData( "r16" ) )
#' msk <- thresholdImage( img, quantile( img[ img > 0 ] )[1], max( img ) )
#' ripped <- ripmmarc( img, msk, patchRadius=3, patchSamples=2000, patchVarEx=0.95  )
#' mdl = lm( t(ripped$imagePatchMat) ~ t( ripped$basisMat  ) )
#' bmdl = bigLMStats( mdl )
#' img[msk==1]=bmdl$beta.t[2,]
#' ripped <- ripmmarc( img, msk, patchRadius=3, patchSamples=2000, patchVarEx=4  )
#' ripped2 <- ripmmarc( img, msk, patchRadius=3, patchSamples=2000,
#'   canonicalFrame = ripped$canonicalFrame,
#'   evecBasis = ripped$basisMat )
#' \dontrun{
#' ripped <- ripmmarc( img, msk, patchRadius=3, patchSamples=2000, patchVarEx=10 )
#' mm = thresholdImage( abs( ripped$canonicalFrame ), 1.e-20, Inf )
#' mm2 = antsImageClone( mm )
#' for (k in 1:nrow( ripped$basisMat ) ) {
#'   mm2[mm==1] =  ripped$basisMat[k,]
#'   plot( mm2, doCropping=F )
#'   }
#' }
#' @export ripmmarc
ripmmarc <- function(
  img,
  mask,
  patchRadius = 3,
  patchSamples = 1000,
  patchVarEx   = 0.95,
  meanCenter   = FALSE,
  canonicalFrame = NA,
  evecBasis    = NA,
  verbose = FALSE  ) {
  print("WARNING: WIP, ripmmarc is not yet implemented all the way!!")
  inimg.float <- antsImageClone( img, "float" )
  mask.float <- antsImageClone( mask, "float" )
  outimg <- antsImageClone( inimg.float )
  if ( any( is.na( evecBasis ) ) ) evecBasis = matrix( nrow=0, ncol=0 )
  else if ( !any( is.na( evecBasis ) ) & is.na( canonicalFrame ) )
    stop("The user must also pass in a canonical frame.")
  if ( is.na( canonicalFrame ) ) {
    canonicalFrame = makeImage( rep( 1, img@dimension ), 0 )
    }
  # FIXME not sure why transpose is needed below ....
  outstruct <- .Call("patchAnalysis",
    inimg.float, mask.float, outimg, patchRadius, patchSamples, patchVarEx,
    meanCenter, canonicalFrame, t(evecBasis), verbose, PACKAGE = "ANTsR")
  outstruct[[1]] = antsImageClone( outstruct[[1]], img@pixeltype )
  return( outstruct )
}
