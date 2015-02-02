#' Simple makeImage function.
#' 
#' Make an image with given size and voxel value or given a mask and vector.
#' 
#' 
#' @param mat input image size or mask
#' @param val input image value or vector, size of mask
#' @return antsImage is output
#' @author Avants BB
#' @examples
#' 
#' outimg<-makeImage( c(2,10) , 1)
#' outimg<-makeImage( outimg ,  c(2,10) )
#' 
#' @export makeImage
makeImage <- function(imagesize, voxval = 1) {
  firstparamnumer <- (typeof(imagesize) == "double" | typeof(imagesize) == "integer")
  if (firstparamnumer) {
    imagedimension <- length(imagesize)
    outimg <- new("antsImage", "float", imagedimension)
    if (imagedimension == 2) 
      ImageMath(imagedimension, outimg, "MakeImage", imagesize[1], imagesize[2])
    if (imagedimension == 3) 
      ImageMath(imagedimension, outimg, "MakeImage", imagesize[1], imagesize[2], 
        imagesize[3])
    if (imagedimension == 4) 
      ImageMath(imagedimension, outimg, "MakeImage", imagesize[1], imagesize[2], 
        imagesize[3], imagesize[4])
    outimg[outimg == 0] <- voxval
    return(outimg)
  }
  if (class(imagesize)[[1]] == "antsImage") {
    img <- antsImageClone(imagesize)
    sel <- (imagesize > 0)
    if (length(voxval) == sum(sel)) 
      img[sel] <- voxval
    return(img)
  }
} 
