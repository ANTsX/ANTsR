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
#' @param components whether there are components per pixel or not
#' @param pixeltype data type of image values
#' @return antsImage is output
#' @author Avants BB
#' @examples
#'
#' outimg<-makeImage( c(2,10) , 1)
#' outimg<-makeImage( outimg ,  c(2,10) )
#' testthat::expect_error(makeImage(outimg, c(2,10), components = 2))
#'
#' @export makeImage
makeImage <- function(imagesize, voxval = 0, spacing=c(NA), origin=c(NA), 
                      direction=c(NA), components = FALSE , pixeltype="float") {

  if ( components > 1 )
    {
    stop("Multichannel images not yet supported")
    }
  if (is.antsImage(imagesize)) {
    img <- antsImageClone(imagesize)
    sel = as.array(imagesize) > 0
    if (length(voxval) == sum(sel) || 
        length(voxval) == 1) {
      img[sel] <- voxval
    } else {
      warning("Number of voxels not the same as the positive values")
    }
    return(img)
  }
  
  firstparamnumer <- (typeof(imagesize) == "double" | typeof(imagesize) == "integer")
  if (firstparamnumer) {
    # imagedimension <- length(imagesize)
    outimg = array(voxval, dim = imagesize)
    args = list(
      object = outimg,
      pixeltype = pixeltype,
      components = components)
    if (!all(is.na(spacing))) {
      args$spacing = spacing
    }
    if (!all(is.na(origin))) {
      args$origin = origin
    }    
    if (!all(is.na(direction))) {
      args$direction = direction
    }
    outimg = do.call("as.antsImage", args = args)
    return(outimg)
  }

}
