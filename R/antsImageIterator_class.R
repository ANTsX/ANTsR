# this file defines the S4 classes related to 'antsImageIterator' and the associated
# methods



#' @rdname antsImageIterator_class
#' @title An S4 class for an antsImageIterator
#'
#' @description C++ type used to represent an ITK image iterator. the
#' actual iterator is of C++ type
#' 'itk::ImageRegionIteratorWithIndex< itk::image< pixeltype, dimension > >'
#'
#' @param object input object to convert
#' @param .Object input object to convert
#' @param pixeltype string e.g. "float" "unsigned char" "int"
#' @param dimension dimensionality of the image
#' @param components number of components per pixel
#' @slot pixeltype usually float, can be other types unsigned char, int, double
#' etc noting that short is not supported
#' @slot dimension usually 2 or 3 but can be 4
#' @slot components number of pixel components, currently only 1 is suppored
#' @slot pointer to the memory location of the itk object
setClass(Class = "antsImageIterator", representation(pixeltype = "character", dimension = "integer",
  components = "integer", pointer = "externalptr"))

#' @rdname antsImageIterator_class
#' @aliases show,antsImageIterator-method
setMethod(f = "show", "antsImageIterator", function(object){
    cat("antsImageIterator\n")
    cat("  Pixel Type          :", object@pixeltype, "\n")
    cat("  Components Per Pixel:", object@components, "\n")
    cat("  Dimensions          :", object@dimension, "\n")
    cat("\n")
})

#' @rdname antsImageIterator_class
#' @aliases initialize,antsImageIterator-method
setMethod(f = "initialize", signature(.Object = "antsImageIterator"), definition = function(.Object,
  pixeltype = "float", dimension = 3, components = 1) {
  ANTsRCore::antsImageIterator(pixeltype, dimension, components)
})

#' @title antsImageIterator
#' @description Get image iterator
#' @param x antsImage
#' @return antsImageIterator
#' @examples
#' img <- makeImage(c(5,5), rnorm(25))
#' it <- antsImageIterator( img )
#' @export
antsImageIterator <- function(x) {
  x = check_ants(x)
  return(ANTsRCore::antsImageIterator(x))
}

#' @title antsImageIteratorGet
#' @description Get value at current position
#' @param x antsImageIterator
#' @return pixel value
#' @examples
#' img <- makeImage(c(5,5), rnorm(25))
#' it <- antsImageIterator( img )
#' value <- antsImageIteratorGet( it )
#' @export
antsImageIteratorGet <- function(x) {
  return(ANTsRCore::antsImageIterator_Get(x))
}

#' @title antsImageIteratorSet
#' @description Set value at current position
#' @param x antsImageIterator
#' @param value pixel value to set
#' @return TRUE'
#' @examples
#' img <- makeImage(c(5,5), rnorm(25))
#' it <- antsImageIterator( img )
#' flag <- antsImageIteratorSet( it, 2.0 )
#' @export
antsImageIteratorSet <- function(x, value) {
  invisible(return(ANTsRCore::antsImageIterator_Set(x, value)))
}

#' @title antsImageIteratorGetIndex
#' @description Get index at current position
#' @param x antsImageIterator
#' @return image index
#' @examples
#' img <- makeImage(c(5,5), rnorm(25))
#' it <- antsImageIterator( img )
#' index <- antsImageIteratorGetIndex( it )
#' @export
antsImageIteratorGetIndex <- function(x) {
  return(ANTsRCore::antsImageIterator_GetIndex(x))
}

#' @title antsImageIteratorSetIndex
#' @description move interator to a given index
#' @param x antsImageIterator
#' @param index index to move to
#' @return TRUE'
#' @examples
#' img <- makeImage(c(5,5), rnorm(25))
#' it <- antsImageIterator( img )
#' flag <- antsImageIteratorSetIndex( it, c(2,3) )
#' @export
antsImageIteratorSetIndex <- function(x, index) {
  invisible(return(ANTsRCore::antsImageIterator_SetIndex(x, index)))
}

#' @title antsImageIteratorNext
#' @description advance iterator forward
#' @param x antsImageIterator
#' @return antsImageIterator
#' @examples
#' img <- makeImage(c(5,5), rnorm(25))
#' it <- antsImageIterator( img )
#' it <- antsImageIteratorNext( it )
#' @export
#'
antsImageIteratorNext<- function(x) {
  return(ANTsRCore::antsImageIterator_Next(x))
}

#' @title antsImageIteratorPrevious
#' @description advance iterator backward
#' @param x antsImageIterator
#' @return antsImageIterator
#' @examples
#' img <- makeImage(c(5,5), rnorm(25))
#' it <- antsImageIterator( img )
#' it <- antsImageIteratorPrevious( it )
#' @export
antsImageIteratorPrevious<- function(x) {
  return(ANTsRCore::antsImageIterator_Previous(x))
}

#' @title antsImageIteratorGoToBegin
#' @description move iterator to begining
#' @param x antsImageIterator
#' @return antsImageIterator
#' @examples
#' img <- makeImage(c(5,5), rnorm(25))
#' it <- antsImageIterator( img )
#' it <- antsImageIteratorNext( it )
#' it <- antsImageIteratorGoToBegin( it )
#' @export
antsImageIteratorGoToBegin<- function(x) {
  return(ANTsRCore::antsImageIterator_GoToBegin(x))
}

#' @title antsImageIteratorIsAtEnd
#' @description test if iterator is at end of data
#' @param x antsImageIterator
#' @return boolean indicating position
#' @examples
#' img <- makeImage(c(5,5), rnorm(25))
#' it <- antsImageIterator( img )
#' it <- antsImageIteratorNext( it )
#' flag <- antsImageIteratorIsAtEnd( it )
#' @export
antsImageIteratorIsAtEnd<- function(x) {
  return(ANTsRCore::antsImageIterator_IsAtEnd(x))
}

#' @title antsImageIteratorGoToReverseBegin
#' @description move iterator to begining of reverse data
#' @param x antsImageIterator
#' @return antsImageIterator
#' @examples
#' img <- makeImage(c(5,5), rnorm(25))
#' it <- antsImageIterator( img )
#' it <- antsImageIteratorGoToReverseBegin( it )
#' @export
antsImageIteratorGoToReverseBegin<- function(x) {
  return(ANTsRCore::antsImageIterator_GoToReverseBegin(x))
}

#' @title antsImageIteratorIsAtReverseEnd
#' @description test if iterator is at end of reverse data
#' @param x antsImageIterator
#' @return boolean indicating position
#' @examples
#' img <- makeImage(c(5,5), rnorm(25))
#' it <- antsImageIterator( img )
#' it <- antsImageIteratorGoToReverseBegin( it )
#' it <- antsImageIteratorPrevious( it )
#' flag <- antsImageIteratorIsAtReverseEnd( it )
#' @export
antsImageIteratorIsAtReverseEnd<- function(x) {
  return(ANTsRCore::antsImageIterator_IsAtReverseEnd(x))
}

#' @title antsImageIteratorRemaining
#' @description test if iterator is at end of data
#' @param x antsImageIterator
#' @return boolean indicating if data remains
#' @examples
#' img <- makeImage(c(5,5), rnorm(25))
#' it <- antsImageIterator( img )
#' it <- antsImageIteratorNext( it )
#' flag <- antsImageIteratorRemaining( it )
#' @export
antsImageIteratorRemaining<- function(x) {
  return(ANTsRCore::antsImageIterator_Remaining(x))
}
