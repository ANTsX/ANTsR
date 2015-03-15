#' crop a sub-image via a mask
#'
#' uses a label image to crop a smaller image from within a larger image
#'
#' @param image antsImage to crop
#' @param labelImage antsImage with label values.
#'  If not supplied, estimated from data.
#' @param label the label value to use
#' @return subimage
#' @author Brian B. Avants, Nicholas J. Tustison
#' @keywords crop, extract sub-image
#' @examples
#'
#' fi <- antsImageRead(getANTsRData("r16"), 2)
#' cropped <- cropImage(fi)
#' cropped <- cropImage(fi, fi, 250)
#'
#' @export cropImage
cropImage <- function( image, labelImage, label=1 ) {
  if(missing(labelImage))
    labelImage <- getMask(image)
  if ( image@pixeltype != "float" | labelImage@pixeltype != "float" ) {
    stop("input images must have float pixeltype")
  }
  .Call("cropImage",
    image, labelImage, label, 0, NULL, NULL, PACKAGE = "ANTsR")
}


#' crop a sub-image by image indices
#'
#' create a proper antsImage sub-image by indexing the image with indices.
#' this is similar to but different from array sub-setting in that the resulting
#' sub-image can be decropped back into its place without having to store its
#' original index locations explicitly.
#'
#' @param image antsImage to crop
#' @param lowerind vector of lower index, should be length image dimensionality
#' @param upperind vector of upper index, should be length image dimensionality
#' @return subimage
#' @author Brian B. Avants, Nicholas J. Tustison
#' @keywords crop, extract sub-image
#' @examples
#'
#' fi <- antsImageRead( getANTsRData("r16") ,2)
#' cropped <- cropIndices( fi, c(2,10), c(5,15) )
#' cropped<-smoothImage( cropped, 5 )
#' decropped<-decropImage( cropped, fi )
#'
#' @export cropIndices
cropIndices <- function( image, lowerind, upperind ) {
  if ( image@pixeltype != "float"  ) {
    stop("input images must have float pixeltype")
  }
  if ( image@dimension != length(lowerind) |
       image@dimension != length(upperind)  )
       stop("dimensionality and index length dont match")
  .Call("cropImage",
    image, image, 1, 2, lowerind, upperind, PACKAGE = "ANTsR")
}


#' decrop a sub-image back into the full image
#'
#' the inverse function for \code{cropImage}
#'
#' @param croppedImage cropped antsImage
#' @param fullImage antsImage to put back into
#' @return decroppedImage
#' @author Brian B. Avants, Nicholas J. Tustison
#' @keywords decrop, extract sub-image
#' @examples
#'
#' fi <- antsImageRead( getANTsRData("r16") ,2)
#' mask <- getMask( fi )
#' cropped <- cropImage( fi, mask, 1 )
#' cropped <- smoothImage( cropped, 1)
#' decropped <- decropImage( cropped , fi )
#'
#' @export decropImage
decropImage <- function( croppedImage, fullImage ) {
  if ( croppedImage@pixeltype != "float" | fullImage@pixeltype != "float" ) {
    stop("input images must have float pixeltype")
  }
  .Call("cropImage",
    croppedImage, fullImage, 1, 1, NULL, NULL, PACKAGE = "ANTsR")
}




#' extract a slice from an image
#'
#' extract a slice from an image and return an image of dimensionality, d-1
#'
#' @param image antsImage to crop
#' @param slice which slice, integer
#' @param direction which axis, integer
#' @return antsImage of dimension - 1
#' @author Brian B. Avants, Nicholas J. Tustison
#' @keywords extract
#' @examples
#'
#' fi <- makeImage( c(10,10,10), rnorm(1000) )
#' slice <- extractSlice( fi, 1, 1 )
#'
#' @export extractSlice
extractSlice <- function( image, slice, direction ) {
  if ( image@pixeltype != "float"  ) {
    stop("input images must have float pixeltype")
  }
  if ( dimension < 3 ) stop("can extract 1-d image")
  if ( direction > image@dimension  )
       stop("dimensionality and index length dont match")
  .Call("extractSlice", image, slice-1, direction-1, PACKAGE = "ANTsR")
}
