#' Image Read
#' 
#' Read an image file into an S4 object of class 'antsImage'.
#' 
#' 
#' @param filename Name of the file to read the image from.
#' @param pixeltype C++ datatype to be used to represent the pixels read. This
#' datatype need not be the same as the datatype used in the file. Allowed
#' values: 'double', 'float' , 'unsigned int' , 'unsigned char'.
#' @param dimension Number of dimensions of the image read. This need not be
#' the same as the dimensions of the image in the file. Allowed values: 2, 3,
#' 4.
#' @return S4 object of Class 'antsImage' -- Success\cr 1 -- Failure
#' @author Shrinidhi KL
#' @examples
#' 
#' \dontrun{
#' img <- antsImageRead( 'imagefile.nii' , dimension = 3  )
#' img <- antsImageRead( 'imagefile.nii' , dimension = 2  )
#' img <- antsImageRead( 'imagefile.nii' , dimension = 4 , pixeltype =  'double' )
#' }
#' 
#' @export antsImageRead
antsImageRead <- function(filename, dimension, pixeltype = "float") {
  if (class(filename) != "character" || length(filename) != 1) {
    print("'filename' argument must be of class 'character' and have length 1")
    return(NULL)
  }
  if (class(pixeltype) != "character" || length(pixeltype) != 1) {
    print("'pixeltype' argument must be of class 'character' and have length 1")
    return(NULL)
  }
  if (((class(dimension) != "numeric") && (class(dimension) != "integer")) || length(dimension) != 
    1) {
    print("'dimension' argument must be of class 'numeric' and have length 1")
    return(NULL)
  }
  rval <- (.Call("antsImageRead", filename, pixeltype, dimension, PACKAGE = "ANTsR"))
  return(rval)
} 
