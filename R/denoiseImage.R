#' Denoise an image
#'
#' Denoise an image using a spatially adaptive filter originally described in J. V.
#' Manjon, P. Coupe, Luis Marti-Bonmati, D. L. Collins, and M. Robles. Adaptive
#' Non-Local Means Denoising of MR Images With Spatially Varying Noise Levels,
#' Journal of Magnetic Resonance Imaging, 31:192-203, June 2010.
#'
#' @param img scalar image to denoise.
#' @param mask optional to limit the denoise region.
#' @param shrinkFactor downsampling level performed within the algorithm.
#' @param p patch radius for local sample. can be a vector.
#' @param r search radius from which to choose extra local samples. can be a vector.
#' @param noiseModel either Rician or Gaussian.
#' @param verbose boolean
#' @return antsImage denoised version of image
#' @author N Tustison, B Avants
#' @examples
#'
#' img <- antsImageRead( getANTsRData("rand")  ) %>% resampleImage( c(32, 32) )
#' dimg <- denoiseImage( img, img * 0 + 1 )
#' dimg2 <- denoiseImage( img)
#'
#' @export denoiseImage
denoiseImage <- function(
  img,
  mask,
  shrinkFactor = 1,
  p = 1,
  r = 3,
  noiseModel = c("Rician","Gaussian"),
  verbose = FALSE )
  {
  img = check_ants(img)

  outimg = antsImageClone( img )
  noiseModel = match.arg(noiseModel)
  mydim = img@dimension
  if ( length( p ) > 1 ) pvar = paste0( p, collapse='x' ) else pvar=p
  if ( length( r ) > 1 ) rvar = paste0( r, collapse='x' ) else rvar=r
  if (  ! missing( mask ) ) {
    mask = check_ants(mask)
    mskIn = antsImageClone( mask, 'unsigned char')
    mskIn = antsCopyImageInfo( img, mskIn )
    myargs <- list(
      d = mydim,
      i = img,
      n = noiseModel[1],
      x = mskIn,
      s = as.numeric( shrinkFactor ),
      p = pvar,
      r = rvar,
      o = outimg,
      v = as.numeric( verbose )
      )
    } else {
      myargs <- list(
        d = mydim,
        i = img,
        n = noiseModel[1],
        s = as.numeric( shrinkFactor ),
        p = pvar,
        r = rvar,
        o = outimg,
        v = as.numeric( verbose )
        )
    }
  ANTsRCore::DenoiseImage(.int_antsProcessArguments( c( myargs ) ))
  return( outimg )
}
