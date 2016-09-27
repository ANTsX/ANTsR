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
#' @param verbose boolean sets verbosity.
#' @return antsImage decomposed bases
#' @author Kandel BM, Avants BB
#' @examples
#' img <- antsImageRead( getANTsRData( "r16" ) )
#' msk <- thresholdImage( img, quantile( img[ img > 0 ] )[1], max( img ) )
#' ripped <- ripmmarc( img, msk, patchRadius=3, patchSamples=200, patchVarEx=0.95  )
#' ripped <- ripmmarc( img, msk, patchRadius=3, patchSamples=200, patchVarEx=4  )
#' @export smoothImage
ripmmarc <- function(
  img,
  mask,
  patchRadius = 3,
  patchSamples = 1000,
  patchVarEx   = 0.95,
  verbose = TRUE  ) {
  print("WARNING: ripmmarc is not yet implemented-WIP all the way!!")
  inimg.float <- antsImageClone( img, "float" )
  mask.float <- antsImageClone( mask, "float" )
  outimg <- antsImageClone( inimg.float )
  outimg <- .Call("patchAnalysis",
    inimg.float, mask.float, outimg, patchRadius, patchSamples, patchVarEx,
    verbose, PACKAGE = "ANTsR")
  return( antsImageClone( outimg, img@pixeltype ) )
}
