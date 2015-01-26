#' Image Write
#' 
#' Write an image object of S4 class 'antsImage' to a file.
#' 
#' 
#' @param image Image object of S4 class 'antsImage' to be written.
#' @param filename Name of the file to write the image to.
#' @return 0 -- Success\cr 1 -- Failure
#' @author Shrinidhi KL
#' @examples
#' 
#' \dontrun{
#' # write an image 'img' of class 'antsImage' to a file named 'imagefile.nii'
#' antsImageWrite( img , "imagefile.nii" )
#' }
#' 
#' @export antsImageWrite
antsImageWrite <- function(image, filename) {
  if (class(image) != "antsImage") {
    print("'image' argument provided is not of class 'antsImage'")
    return(NULL)
  }
  return(.Call("antsImageWrite", image, filename, PACKAGE = "ANTsR"))
} 
