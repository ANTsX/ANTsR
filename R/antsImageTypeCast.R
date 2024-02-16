#' Cast images to either a passed pixeltype or the most precise of the inputs
#'
#' @param imageList list of images
#' @param pixeltype to cast to
#' @return list of antsImages
#' @author J Duda
#' @examples
#'
#' img <- antsImageRead( getANTsRData("rand")  ) %>% resampleImage( c(32, 32) )
#' img2 <- antsImageRead( getANTsRData("rand"), pixeltype="double"  ) %>% 
#' resampleImage( c(32, 32) )
#' allDoubles = antsImageTypeCast( list(img, img2) )
#' floats = antsImageTypeCast( list(img) )
#' pixeltype(img) = "unsigned int"
#' ints = antsImageTypeCast( list(img) )
#'
#' @export
antsImageTypeCast <- function( imageList, pixeltype=NA ) {

  pixTypes = c()
  for ( i in imageList ) {
    pixTypes = c(pixTypes, i@pixeltype)
  }

  if ( is.na(pixeltype) ) {
    pixeltype = "unsigned char"
    for ( i in pixTypes ) {
      if (i == "double") {
        pixeltype="double"
      }
      else if ( (i=="float") & (pixeltype!="double") ) {
        pixeltype="float"
      }
      else if ( (i=="unsigned int") & (pixeltype!="double") & (pixeltype!="float") ) {
        pixeltype="unsigned int"
      }
    }
  }

  outImages = list()
  count = 1
  for ( i in imageList ) {
    if ( pixTypes[count] == pixeltype) {
      outImages[[count]] = i
    }
    else {
      outImages[[count]] = antsImageClone(i, out_pixeltype=pixeltype)
    }
    count = count + 1
  }

  return(outImages)
}
