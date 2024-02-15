# this file defines the S4 classes related to 'antsImage' and their associated
# methods

#' An S4 class to hold a region of an antsImage
#'
#' @slot index indices of the region
#' @slot size size of the region
#' @useDynLib ANTsRCore
#' @import Rcpp
#' @import ITKR
#' @import methods
#' @import tools
#' @import stats
setClass(Class = "antsRegion", representation(index = "numeric", size = "numeric"))
##### @useDynLib ANTsRCore, .registration = TRUE


#' An S4 class for an antsImage
#'
#' C++ type used to represent an ITK image pointed to by 'pointer'. the
#' actual image is of C++ type 'itk::image< pixeltype , dimension >::Pointer'
#'
#' @param object input object to convert
#' @param .Object input object to convert
#' @param pixeltype usually float, can be other types unsigned char, int, double
#' etc noting that short is not supported
#' @param dimension usually 2 or 3 but can be 4
#' @param components number of pixel components
#' @param isVector logical indicator of the image is a vector
#' @param filename character filename if the data was read in, otherwise
#' ""
#'
#' @slot pixeltype usually float, can be other types unsigned char, int, double
#' etc noting that short is not supported
#' @slot dimension usually 2 or 3 but can be 4
#' @slot components number of pixel components
#' @slot pointer the memory location
#' @slot isVector logical indicator of the image is a vector
#' @slot filename character filename if the data was read in, otherwise
#' ""
#' @rdname antsImage
setClass(Class = "antsImage",
         representation(pixeltype = "character", dimension = "integer",
                        components = "integer", pointer = "externalptr",
                        isVector = "logical",
                        filename = "character"),
         prototype=list(isVector=FALSE,
                        filename = "") )

#' @rdname antsImage
#' @aliases show,antsImage-method
setMethod(f = "show", "antsImage", function(object){
  cat("antsImage\n")
  cat("  Pixel Type          :", object@pixeltype, "\n")
  cat("  Components Per Pixel:", object@components, "\n")
  cat("  Dimensions          :", paste(dim(object), collapse="x"), "\n")
  cat("  Voxel Spacing       :", paste(antsGetSpacing(object), collapse="x"), "\n")
  cat("  Origin              :", antsGetOrigin(object), "\n")
  cat("  Direction           :", antsGetDirection(object), "\n")
  fname = slot(object, "filename")
  if (length(fname) > 0){
    if (fname != "") {
      cat("  Filename           :", fname, "\n")
    }
  }
  cat("\n")
})
#' @rdname antsImage
#' @aliases initialize,antsImage-method
#' @slot pixeltype usually float, can be other types unsigned char, int, double
#' etc noting that short is not supported
#' @slot dimension usually 2 or 3 but can be 4
#' @slot components number of pixel components
#' @slot isVector boolean
#' @slot filename character filename if the data was read in, otherwise
#' ""
setMethod(f = "initialize", signature(.Object = "antsImage"),
          definition = function(.Object,
                                pixeltype = "float", dimension = 3, components = 1,
                                isVector=FALSE,
                                filename = "") {
            return(.Call("antsImage", pixeltype, dimension, components, PACKAGE = "ANTsRCore"))
          })

#' @rdname as.array
#' @name as.array
#' @aliases dim,antsImage-method
#' @export
setMethod(f = "dim", signature(x = "antsImage"), definition = function(x) {
  return(.Call("antsImage_dim", x, PACKAGE = "ANTsRCore"))
})


# #' @rdname rm.antsImage
# #' @name rm.antsImage
# #' @aliases rm,antsImage-method
# #' @export
#rm.antsImage <- function(x) {
#  print("rm.antsImage(x)")
#  a = .Call("antsImage_rm", x, PACKAGE = "ANTsRCore")
#  print("done with call to antsImage_rm")
#  #x@pointer = NA
#  rm(x)
#  return(NA)
#}

###  @  rdname antsImage # this doesnt work b/c nnz is not a generic
# setGeneric("nnz",function(x){standardGeneric("nnz")})
# setMethod(f = "nnz", signature(x = "antsImage"), definition = function(x) {
#   return(length( which( as.array(x) != 0)))
# })

# #' @rdname as.array
# #' @aliases is.na,antsImage-method .
# #' @export
# #' @examples
# #' outimg<-makeImage( c(2,10) , 1)
# #' is.na(outimg)
# setMethod(f = "is.na", signature(x = "antsImage"), definition = function(x) {
#   val <- .Call("antsImage_isna", x, PACKAGE = "ANTsRCore")
#   if (val > 0) {
# return(TRUE)
#   }
#   return(FALSE)
# })

#' @rdname as.array
#' @aliases as.numeric,antsImage-method
#' @param mask a logical vector/array or binary antsImage object
#' @param region a \code{antsRegion} object
#' @export
#' @examples
#' outimg<-makeImage( c(2,10) , rnorm(20))
#' as.numeric(outimg)
#' as.numeric(outimg, mask = outimg > 1)
#'  testthat::expect_error(as.numeric(outimg, mask = outimg))
#'
setMethod(f = "as.numeric", signature(x = "antsImage"),
          definition = function(x,
                                mask = logical(),
                                region = new("antsRegion",
                                             index = integer(), size = integer())
          ) {
            mask = c(coerce_mask(mask))
            if (typeof(mask) != "logical") {
              stop("'mask' provided is not of type 'logical'")
            }
            num = .Call("antsImage_asVector", x, mask, region, PACKAGE = "ANTsRCore")
            num = as.numeric(num)
            return(num)
          })

#' @rdname as.array
#' @aliases as.matrix,antsImage-method
#' @export
#' @examples
#' outimg<-makeImage( c(2,10) , rnorm(20))
#' as.matrix(outimg)
#' as.matrix(outimg, mask = outimg > 1)
#' testthat::expect_error(as.matrix(outimg, mask = outimg) )
#' outimg<-makeImage( c(2,10,2) , rnorm(40))
#' testthat::expect_error(as.matrix(outimg) )
setMethod(f = "as.matrix", signature(x = "antsImage"),
          definition = function(x, mask = logical(),
                                region = new("antsRegion", index = integer(), size = integer())) {
            mask = c(coerce_mask(mask))
            if (typeof(mask) != "logical") {
              stop("'mask' provided is not of type 'logical'")
            }
            if (x@dimension != 2) {
              stop("image dimension must be 2")
            }
            return(.Call("antsImage_asVector", x, mask, region, PACKAGE = "ANTsRCore"))
          })

#' @rdname as.array
#' @export
#' @method as.matrix antsImage
as.matrix.antsImage = function(x, ...,
                               mask = logical(),
                               region = new("antsRegion", index = integer(),
                                            size = integer())) {
  mask = c(coerce_mask(mask))
  if (typeof(mask) != "logical") {
    stop("'mask' provided is not of type 'logical'")
  }
  if (x@dimension != 2) {
    stop("image dimension must be 2")
  }
  return(.Call("antsImage_asVector", x, mask, region, PACKAGE = "ANTsRCore"))
}

#' @title Coerce antsImage objects to array
#' @description Converts antsImage, antsImage object to different data types
#' @rdname as.array
#' @param x object of class \code{antsImage} or \code{antsMatrix}
#' @param ... additional arguments passed to functions
#' @aliases as.array,antsImage-method
setMethod(f = "as.array", signature(x = "antsImage"),
          definition = function(x, ..., mask = logical(),
                                region = new("antsRegion", index = integer(), size = integer())) {
            mask = c(coerce_mask(mask))
            if (typeof(mask) != "logical") {
              stop("'mask' provided is not of type 'logical'")
            }
            return(.Call("antsImage_asVector", x, mask, region, PACKAGE = "ANTsRCore"))
          })

#' @rdname as.array
#' @export
#' @method as.array antsImage
#' @examples
#' outimg<-makeImage( c(2,10) , rnorm(20))
#' as.matrix(outimg)
#' as.matrix(outimg, mask = outimg > 1)
#' outimg<-makeImage( c(2,10,2) , rnorm(40))
#' testthat::expect_error(as.matrix(outimg) )
as.array.antsImage = function(x, ..., mask = logical(),
                              region = new("antsRegion", index = integer(),
                                           size = integer())) {
  mask = c(coerce_mask(mask))
  if (typeof(mask) != "logical") {
    stop("'mask' provided is not of type 'logical'")
  }
  return(.Call("antsImage_asVector", x, mask, region, PACKAGE = "ANTsRCore"))
}

#' @rdname as.array
#' @export
#' @method as.character antsImage
#' @examples
#' img <- antsImageRead( getANTsRData( "r16" ) )
#' img[img > 5] = 0
#' sort(unique(as.character(img)))
#' factor(img)
#' table(img, img)
as.character.antsImage = function(x, ..., mask = logical(),
                                  region = new("antsRegion", index = integer(),
                                               size = integer())) {
  arr = as.array(x, ..., mask = mask,
                 region = region)
  return(as.character(arr))
}


#' @rdname as.array
#' @export
#' @method as.factor antsImage
as.factor.antsImage = function(
  x, ..., mask = logical(),
  region = new("antsRegion", index = integer(),
               size = integer())) {
  arr = as.array(x, ..., mask = mask,
                 region = region)
  return(as.factor(arr))
}

#' @rdname as.array
#' @export
#' @method factor antsImage
factor.antsImage = function(
  x, ..., mask = logical(),
  region = new("antsRegion", index = integer(),
               size = integer())) {
  arr = as.character(x, mask = mask, region)
  return(factor(arr, ...))
}



#' @rdname as.antsImage
#' @title Convert Objects into antsImage objects
#'
#' @description Convert types to antsImage
#'
#' @param object An object
#' @param pixeltype a character string e.g. "float"
#' @param spacing numeric vector matching image dimensionality e.g. c(1.2,1.2)
#' @param origin numeric vector matching image dimensionality e.g. c(0,0)
#' @param direction numeric matrix matching image dimensionality eg diag(2)
#' @param components number of components per voxel, e.g. 1
#' @param reference optional reference antsImage providing all header info
#' @param ... Extra named arguments passed to FUN
#' @export
setGeneric(name = "as.antsImage", def = function(
  object,
  pixeltype = "float",
  spacing = as.numeric(seq.int(from = 1, by = 0, length.out = length(dim(object)))),
  origin = as.numeric(seq.int(from = 0, by = 0, length.out = length(dim(object)))),
  direction = diag(length(dim(object))),
  components = FALSE, reference = NULL, ...) {
  standardGeneric("as.antsImage")
})

#' @rdname as.antsImage
#' @aliases as.antsImage,matrix-method
setMethod(f = "as.antsImage", signature(object = "matrix"), definition = function(object,
                                                                                  pixeltype = "float", spacing = as.numeric(seq.int(from = 1, by = 0, length.out = length(dim(object)))),
                                                                                  origin = as.numeric(seq.int(from = 0, by = 0, length.out = length(dim(object)))),
                                                                                  direction = diag(length(dim(object))), components = FALSE, reference = NULL) {
  if ( is.antsImage(reference) )
  {
    pixeltype = reference@pixeltype
    components = (reference@components > 1)
    ndim = length(dim(object))
    spacing = antsGetSpacing(reference)[seq_len(ndim)]
    origin = antsGetOrigin(reference)[seq_len(ndim)]
    direction = antsGetDirection(reference)
  }
  return(.Call("antsImage_asantsImage", object, pixeltype, spacing, origin, direction, components, PACKAGE = "ANTsRCore"))
})

#' @rdname as.antsImage
#' @aliases as.antsImage,array-method
#' @examples
#' arr = array(rnorm(10^3), dim = rep(10, 3))
#' img = as.antsImage(arr)
#' i2 = as.antsImage(arr, reference = img)
setMethod(f = "as.antsImage", signature(object = "array"), definition = function(object,
                                                                                 pixeltype = "float", spacing = as.numeric(seq.int(from = 1, by = 0, length.out = length(dim(object)))),
                                                                                 origin = as.numeric(seq.int(from = 0, by = 0, length.out = length(dim(object)))),
                                                                                 direction = diag(length(dim(object))), components = FALSE, reference = NULL) {
  if ( is.antsImage(reference) )
  {
    pixeltype = reference@pixeltype
    components = (reference@components > 1)
    ndim = length(dim(object))
    spacing = antsGetSpacing(reference)[seq_len(ndim)]
    origin = antsGetOrigin(reference)[seq_len(ndim)]
    direction = antsGetDirection(reference)
  }
  return(.Call("antsImage_asantsImage", object, pixeltype, spacing, origin, direction, components, PACKAGE = "ANTsRCore"))
})

#' @title is.antsImage
#'
#' @description Tests if object is antsImage
#'
#' @param x An object
#' @return TRUE if \code{object} is antsImage; FALSE otherwise.
#' @examples
#' is.antsImage(antsImageRead(getANTsRData('r16'), 2))
#' @export
is.antsImage <- function(x){
  inherits(x, 'antsImage')
}
