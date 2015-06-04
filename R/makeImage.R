#' Simple makeImage function.
#'
#' Make an image with given size and voxel value or given a mask and vector.
#'
#'
#' @param imagesize input image size or mask
#' @param voxval input image value or vector, size of mask
#' @param spacing image spatial resolution
#' @param origin image spatial origin
#' @param direction direction matrix to convert from index to physical space
#' @param components components per pixel
#' @param pixeltype data type of image values
#' @return antsImage is output
#' @author Avants BB
#' @examples
#'
#' outimg<-makeImage( c(2,10) , 1)
#' outimg<-makeImage( outimg ,  c(2,10) )
#'
#' @export makeImage
makeImage <- function(imagesize, voxval = 0, spacing=c(NA), origin=c(NA), direction=c(NA), components=1 , pixeltype="float") {

  if ( components != 1 )
    {
    stop("Multichannel images not yet supported")
    }

  firstparamnumer <- (typeof(imagesize) == "double" | typeof(imagesize) == "integer")
  if (firstparamnumer) {
    imagedimension <- length(imagesize)

    if ( is.na(spacing[1]))
      {
      spacing = rep(1, imagedimension)
      }
    if ( is.na(origin[1]))
      {
      origin = rep(0, imagedimension)
      }
    if ( is.na(direction[1]))
      {
      direction = matrix(data=0, nrow=imagedimension, ncol=imagedimension)
      diag(direction) = rep(1, imagedimension)
      }

    outimg = .Call( "makeImage", pixeltype, imagesize, spacing, origin, direction, components, PACKAGE="ANTsR" )

    if ( length(voxval) == 1)
      {
      outimg[outimg == 0] = voxval
      }
    else if ( length(voxval) > 1 )
      {
      outimg[outimg == 0] <- voxval
      }

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
