#' @name check_ants-methods
#' @docType methods
#' @aliases check_ants
#' @title Check if antsImage or read in
#' @description Simple check to see if input is character, list, \code{nifti},
#' or class \code{antsImage}
#' @return antsImage object
#' @seealso \code{\link{antsImageRead}}
#' @param x character path of image or
#' an object of class antsImage
#' @param ... arguments passed to other methods,
#' namely \code{\link{antsImageRead}}
#' @export
#' @import methods
#' @author John Muschelli \email{muschellij2@@gmail.com}
setGeneric("check_ants", function(x, ...) {
  standardGeneric("check_ants")
})

#' @rdname check_ants-methods
#' @aliases check_ants,antsImage-method
#' @export
setMethod("check_ants", "antsImage", function(x, ...) {
  # x2 = antsImageClone(x)
  # return(x2)
  return(x)
})

#' @rdname check_ants-methods
#' @aliases check_ants,ANY-method
#' @export
setMethod("check_ants", "ANY", function(x, ...) {
  # just return the thing
  return(x)
})

#' @rdname check_ants-methods
#' @aliases check_ants,character-method
#'
#' @export
setMethod("check_ants", "character", function(x, ...) {
  ### add vector capability
  if (length(x) > 1) {
    file <- lapply(x, check_ants, ... = ...)
    return(file)
  } else {
    img <- antsImageRead(x, ...)
    return(img)
  }
})


#' @rdname check_ants-methods
#' @aliases check_ants,list-method
#' @export
setMethod("check_ants", "list", function(x, ...) {
  ### add vector capability
  file <- lapply(x, check_ants, ... = ...)
  return(file)
})
