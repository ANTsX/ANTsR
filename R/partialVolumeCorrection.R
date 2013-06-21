partialVolumeCorrection <- function(img, img.gm, img.wm, mask=NULL, proportion=0.4){
  #  WIP -- the following does not appear to work, possibly due to a bug in ImageMath. 
  #  It may be platform-dependent.
  img.corrected <- antsImageClone(img)
  img.denominator <- antsImageClone(img.gm)
  denom.wm <- antsImageClone(img.wm)
  ImageMath(img@dimension, denom.wm, 'm', img.wm, '0.4')
  ImageMath(img@dimension, img.denominator, '+', img.gm, denom.wm)
  ImageMath(img@dimension, img.corrected, "/", img, img.denominator)
  if(!is.null(mask)) img.corrected <- maskImage(img.corrected, mask) 
  img.corrected
}
