getMask <- function(img = NULL, lowThresh = 1, highThresh = Inf, 
                    cleanup = 2 ) {
  # Binarizes a mask between specified thresholds Input can be a file name or an
  # antsImage, if it is not specified, a file chooser is launched. Works on 3D
  # images only If cleanup == TRUE, small and weakly-connected elements are removed
  # by erosion, and then holes are filled.  Returns: a binary antsImage
  
  if (is.character(img)) {
    if (length(img) != 1) {
      stop("'img' must be a single filename")
    }
    img <- antsImageRead(img, 3, "float")
  } else if (class(img) == "antsImage") {
    if (img@pixeltype != "float") {
      img <- antsImageClone(img, "float")
    }
  } else {
    img <- file.choose()
  }
  
  if ((!is.numeric(lowThresh)) || (!is.numeric(highThresh)) || length(lowThresh) > 
    1 || length(highThresh) > 1) {
    stop("'lowthresh' and 'highthresh' must be numeric scalars")
  }
  
  
  mask_img <- new("antsImage", "float", img@dimension)
  
  ThresholdImage(img@dimension, img, mask_img, lowThresh, highThresh)
  
  if (cleanup > 0 ) {
    ImageMath(img@dimension, mask_img, "ME", mask_img, cleanup)
    ImageMath(img@dimension, mask_img, "GetLargestComponent", mask_img)
    ImageMath(img@dimension, mask_img, "MD", mask_img, cleanup-1)
    ImageMath(img@dimension, mask_img, "FillHoles", mask_img)
  }
  
  return(mask_img)
  
} 
