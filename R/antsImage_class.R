# this file defines the S4 classes related to 'antsImage' and their associated
# methods

#' An S4 class to hold a region of an antsImage
#'
#' @slot index indices of the region
#' @slot size size of the region
#' @useDynLib ANTsR
#' @import Rcpp
#' @import methods
#' @import tools
#' @import stats
setClass(Class = "antsRegion", representation(index = "numeric", size = "numeric"))


#' An S4 class for an antsImage
#'
#' C++ type used to represent an ITK image pointed to by 'pointer'. the
#' actual image is of C++ type 'itk::image< pixeltype , dimension >::Pointer'
#'
#' @param object input object to convert
#' @param .Object input object to convert
#' 
#' @slot pixeltype usually float, can be other types unsigned char, int, double
#' etc noting that short is not supported
#' @slot dimension usually 2 or 3 but can be 4
#' @slot components number of pixel components
#' @slot pointer the memory location
#' @rdname antsImage
setClass(Class = "antsImage", 
         representation(pixeltype = "character", dimension = "integer",
  components = "integer", pointer = "externalptr"))

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
    cat("\n")
})
#' @rdname antsImage
#' @aliases initialize,antsImage-method 
setMethod(f = "initialize", signature(.Object = "antsImage"), definition = function(.Object,
  pixeltype = "float", dimension = 3, components = 1) {
  return(.Call("antsImage", pixeltype, dimension, components, PACKAGE = "ANTsR"))
})

#' @rdname as.array
#' @aliases dim,antsImage-method 
#' @export 
setMethod(f = "dim", signature(x = "antsImage"), definition = function(x) {
  return(.Call("antsImage_dim", x, PACKAGE = "ANTsR"))
})



###  @  rdname antsImage # this doesnt work b/c nnz is not a generic
# setGeneric("nnz",function(x){standardGeneric("nnz")})
# setMethod(f = "nnz", signature(x = "antsImage"), definition = function(x) {
#   return(length( which( as.array(x) != 0)))
# })

#' @rdname as.array
#' @aliases is.na,antsImage-method .
#' @export 
setMethod(f = "is.na", signature(x = "antsImage"), definition = function(x) {
  val <- .Call("antsImage_isna", x, PACKAGE = "ANTsR")
  if (val > 0) {
    return(TRUE)
  }
  return(FALSE)
})

#' @rdname as.array
#' @aliases as.numeric,antsImage-method 
#' @export  
setMethod(f = "as.numeric", signature(x = "antsImage"), definition = function(x,
  mask = logical(), region = new("antsRegion", index = integer(), size = integer())) {
  if (typeof(mask) != "logical") {
    stop("'mask' provided is not of type 'logical'")
  }
  num = .Call("antsImage_asVector", x, mask, region, PACKAGE = "ANTsR")
  num = as.numeric(num)
  return(num)
})

#' @rdname as.array
#' @aliases as.matrix,antsImage-method 
#' @export  
setMethod(f = "as.matrix", signature(x = "antsImage"),
 definition = function(x, mask = logical(),
  region = new("antsRegion", index = integer(), size = integer())) {
  if (typeof(mask) != "logical") {
    stop("'mask' provided is not of type 'logical'")
  }
  if (x@dimension != 2) {
    stop("image dimension must be 2")
  }
  return(.Call("antsImage_asVector", x, mask, region, PACKAGE = "ANTsR"))
})

#' @rdname as.array
#' @export
as.matrix.antsImage = function(x, mask = logical(),
                              region = new("antsRegion", index = integer(), 
                                           size = integer())) {
  if (typeof(mask) != "logical") {
    stop("'mask' provided is not of type 'logical'")
  }
  if (x@dimension != 2) {
    stop("image dimension must be 2")
  }
  return(.Call("antsImage_asVector", x, mask, region, PACKAGE = "ANTsR"))
}

#' @title Coerce antsImage objects to array 
#' @description Converts antsImage, antsImage object to different data types
#' @rdname as.array
#' @param x object of class \code{\link{antsImage}} or \code{\link{antsMatrix}}
#' @aliases as.array,antsImage-method 
setMethod(f = "as.array", signature(x = "antsImage"),
 definition = function(x, mask = logical(),
  region = new("antsRegion", index = integer(), size = integer())) {
  if (typeof(mask) != "logical") {
    stop("'mask' provided is not of type 'logical'")
  }
  return(.Call("antsImage_asVector", x, mask, region, PACKAGE = "ANTsR"))
})

#' @rdname as.array
#' @export
as.array.antsImage = function(x, mask = logical(),
                              region = new("antsRegion", index = integer(), 
                                           size = integer())) {
  if (typeof(mask) != "logical") {
    stop("'mask' provided is not of type 'logical'")
  }
  return(.Call("antsImage_asVector", x, mask, region, PACKAGE = "ANTsR"))
}

#' Get Pixels
#'
#' Get pixel values from an 'antsImage'.
#'
#'
#' @param x Image object of S4 class 'antsImage' to get values from.
#' @param i indices in first dimension
#' @param j indices in second dimension
#' @param k indices in third dimension
#' @param l indices in forth dimension
#' @return array of pixel values
#' @examples
#'
#' img<-makeImage(c(10,10),rnorm(100))
#' pixel<-getPixels(img,i=c(1,2),j=1)
#'
#'
#' @export getPixels
getPixels <- function(x, i = NA, j = NA, k = NA, l = NA) {
  lst <- NULL
  if (length(i) != 1 || !is.na(i)) {
    if (is.null(i)) {
      lst <- c(lst, list(integer(0)))
    } else if (class(i) == "integer" || class(i) == "numeric") {
      lst <- c(lst, list(i))
    } else {
      stop("indices must be of class 'integer' or 'numeric'")
    }
  }

  if (length(j) != 1 || !is.na(j)) {
    if (is.null(j)) {
      lst <- c(lst, list(integer(0)))
    } else if (class(j) == "integer" || class(j) == "numeric") {
      lst <- c(lst, list(j))
    } else {
      stop("indices must be of class 'integer' or 'numeric'")
    }
  }

  if (length(k) != 1 || !is.na(k)) {
    if (is.null(k)) {
      lst <- c(lst, list(integer(0)))
    } else if (class(k) == "integer" || class(k) == "numeric") {
      lst <- c(lst, list(k))
    } else {
      stop("indices must be of class 'integer' or 'numeric'")
    }
  }

  if (length(l) != 1 || !is.na(l)) {
    if (is.null(l)) {
      lst <- c(lst, list(integer(0)))
    } else if (class(l) == "integer" || class(l) == "numeric") {
      lst <- c(lst, list(l))
    } else {
      stop("indices must be of class 'integer' or 'numeric'")
    }
  }
  return(.Call("antsImage_GetPixels", x, lst, PACKAGE = "ANTsR"))
}

#' @title antsImageGetSet
#' @description Get and set methods for image header information
#' @name antsImageGetSet
#' @examples
#' img <- makeImage(c(5,5), rnorm(25))
#' antsSetSpacing(img, c(2.0, 2.0))
#' antsSetOrigin(img, c(0.5, 0.5))


#' @rdname antsImageGetSet
#' @usage antsGetSpacing(x)
#' @param x antsImage to access, of dimensionality \code{d}.
#' @return For \code{get} methods, vector of length \code{d} (origin, spacing) or matrix of size \code{d * d} (direction).
#' For \code{set} methods, 0 to indicate success.
antsGetSpacing <- function(x) {
  if (class(x)[1] != "antsImage") {
    stop("Input must be of class 'antsImage'")
  }

  return(.Call("antsImage_GetSpacing", x, PACKAGE = "ANTsR"))
}
#' @rdname antsImageGetSet
#' @param spacing numeric vector of length \code{d}.
antsSetSpacing <- function(x, spacing) {
  if (class(x)[1] != "antsImage") {
    stop("Input must be of class 'antsImage'")
  }

  if ((class(spacing) != "numeric") && (class(spacing) != "array")) {
    stop("spacing must be of class 'numeric'")
  }

  if (length(spacing) != length(dim(x))) {
    stop("spacing must be of same dimensions as image")
  }

  return(.Call("antsImage_SetSpacing", x, spacing, PACKAGE = "ANTsR"))
}

#' @rdname antsImageGetSet
#' @usage antsGetOrigin(x)
antsGetOrigin <- function(x) {
  if (class(x)[1] != "antsImage") {
    stop("Input must be of class 'antsImage'")
  }
  return(.Call("antsImage_GetOrigin", x, PACKAGE = "ANTsR"))
}
#' @rdname antsImageGetSet
#' @usage antsSetOrigin(x, origin)
#' @param origin numeric vector of length \code{d}.
antsSetOrigin <- function(x, origin) {
  if (class(x)[1] != "antsImage") {
    stop("Input must be of class 'antsImage'")
  }
  if ((class(origin) != "numeric") && (class(origin) != "array")) {
    stop("spacing must be of class 'numeric' or 'array'")
  }

  if (length(origin) != length(dim(x))) {
    stop("spacing must be of same dimensions as image")
  }

  return(.Call("antsImage_SetOrigin", x, origin, PACKAGE = "ANTsR"))
}

#' @rdname antsImageGetSet
#' @usage antsGetDirection(x)
antsGetDirection <- function(x) {
  if (class(x)[1] != "antsImage") {
    stop("Input must be of class 'antsImage'")
  }
  return(.Call("antsImage_GetDirection", x, PACKAGE = "ANTsR"))
}

#' @rdname antsImageGetSet
#' @usage antsSetDirection(x, direction)
#' @param direction matrix of size \code{d * d}.
antsSetDirection <- function(x, direction) {
  if (class(x)[1] != "antsImage") {
    stop("Input must be of class 'antsImage'")
  }
  if ((class(direction) != "matrix") & (class(direction) != "array")) {
    stop("direction must be of class 'matrix' or 'array'")
  }
  if ((dim(direction)[1] != length(dim(x))) || (dim(direction)[2] != length(dim(x)))) {
    stop("direction matrix must be of size imagedim * imagedim")
  }
  return(.Call("antsImage_SetDirection", x, direction, PACKAGE = "ANTsR"))
}


#' Get a hypercube neighborhood at a voxel
#'
#' Get the values in a local neighborhood of an \code{antsImage}.
#'
#' @param image Image object of S4 class \code{antsImage} to get values from.
#' @param center array of indices for neighborhood center
#' @param kernel either an array of values for neighborhood radius (in voxels) or a binary array of the same dimension as the image, specifying the shape of the neighborhood to extract
#' @param physical.coordinates a logical indicating if voxel indices and
#' offsets should be in voxel or physical coordinates
#' @return a list
#' \itemize{
#'   \item{values}{numeric vector of values}
#'   \item{indices}{matrix providing the coordinates for each value}
#' }
#' @author Duda JT
#' @examples
#'
#' img<-makeImage(c(10,10),rnorm(100))
#' center <- dim(img)/2
#' radius <- rep(3,2)
#' nhlist <- getNeighborhoodAtVoxel(img,center,radius)
#' kernel <- 1*(rnorm(49)>0)
#' dim(kernel) <- c(7,7)
#' randlist <- getNeighborhoodAtVoxel(img,center,kernel)
#'
#' @export getNeighborhoodAtVoxel
getNeighborhoodAtVoxel <- function(image, center, kernel, physical.coordinates = FALSE ) {

  if (class(image)[1] != "antsImage") {
    stop("Input must be of class 'antsImage'")
  }

  if ((class(center) != "numeric")) {
    stop("center must be of class 'numeric'")
  }

  radius = dim(kernel)
  if ( is.null(radius) ) {
    kernelSize = 2*kernel+1
    kernel = rep(1, prod(kernelSize))
    dim(kernel) = kernelSize
    radius = (kernelSize-1)/2
    }
  else {
    # Check that all sizes are odd
    radius = (dim(kernel)-1)/2
  }

  if ( length(dim(kernel)) != image@dimension ) {
    stop("kernel must have same number of dimensions as 'image'")
  }

  return(.Call("antsImage_GetNeighborhood", image, center, kernel, radius,
               physical.coordinates, PACKAGE="ANTsR"))
}


#' @name getNeighborhoodInMask
#' @title Get neighborhoods for voxels within mask
#'
#' @description this converts a scalar image to a matrix with rows that contain neighbors
#' around a center voxel
#'
#' @param image image object of S4 class \code{antsImage} to get values from.
#' @param mask image object of S4 class \code{antsImage} indicating which voxels to
#' examine. Each voxel > 0 will be used as the center of a neighborhood
#' @param radius array of values for neighborhood radius (in voxels)
#' @param physical.coordinates logical indicating if voxel indices and
#' offsets should be in voxel or physical coordinates
#' @param boundary.condition  string indicating how to handle voxels in a
#' neighborhood, but not in the mask. See \code{Details}.
#' @param spatial.info a boolean indicating of voxel locations and neighborhood
#' offsets should be returned along with pixel values.
#' @param get.gradient a boolean indicating if a matrix of gradients (at the
#' center voxel) should be returned in addition to the value matrix (WIP)
#' @details
#' \code{boundary.condition} should be one of:
#' \itemize{
#'   \item{\code{NA}: }{Fill values with \code{NA}.}
#'   \item{\code{image}: }{Use image value, even if not in mask.}
#'   \item{\code{mean}: }{Use man of all non-\code{NA} values for that neighborhood.}
#' }
#' @return
#'
#' if \code{spatial.info} is false: a matrix of pixel values where the number of rows
#' is the size of the neighborhood and there is a column for each voxel
#'
#' if \code{spatial.info} is true, a list containing three matrices:
#' \itemize{
#'  \item{values: }{matrix of pixel values where the number of rows
#'  is the size of the neighborhood and there is a column for each voxel.}
#'  \item{indices: }{matrix providing the center coordinates for each neighborhood}
#'  \item{offsets: }{matrix providing the offsets from center for each
#'   voxel in a neighborhood}
#' }
#' @author Duda JT
#' @examples
#'
#' r16 <- getANTsRData("r16")
#' r16 <- antsImageRead(r16,2)
#' mask <- getMask(r16,lowThresh=mean(r16),cleanup=1)
#' radius <- rep(2,2)
#' mat <- getNeighborhoodInMask(r16,mask,radius)
#'
#'
#' @export getNeighborhoodInMask
getNeighborhoodInMask <- function(image, mask, radius, physical.coordinates = FALSE,
  boundary.condition = "NA", spatial.info = FALSE, get.gradient = FALSE ) {

  if (class(image)[1] != "antsImage") {
    stop("Input must be of class 'antsImage'")
  }

  if ((class(mask) != "antsImage")) {
    stop("center must be of class 'antsImage'")
  }

  if ((class(radius) != "numeric")) {
    stop("radius must be of class 'numeric'")
  }

  if ((prod(radius * 2 + 1) * sum(as.array(mask))) > (2^31 - 1)) {
    stop("Requested matrix size is too large for Rcpp")
  }

  boundary = 0
  if (boundary.condition == "image") {
    boundary = 1
  }
  if (boundary.condition == "mean") {
    boundary = 2
  }

  return(.Call("antsImage_GetNeighborhoodMatrix", image, mask, radius, physical.coordinates,
    boundary, spatial.info, get.gradient ))

}

.getValueAtPoint <- function(x, point) {
  if (class(x)[1] != "antsImage") {
    stop("Input must be of class 'antsImage'")
  }
  if ((class(point) != "numeric")) {
    stop("point must be of class 'numeric'")
  }

  idx <- as.numeric(antsTransformPhysicalPointToIndex(x, point))
  idx <- floor(idx)

  dims <- length(idx)

  value <- NA
  if (dims == 2) {
    value <- getPixels(x, i = idx[1], j = idx[2])
  } else if (dims == 3) {
    value <- getPixels(x, i = idx[1], j = idx[2], k = idx[3])
  } else if (dims == 4) {
    value <- getPixels(x, i = idx[1], j = idx[2], k = idx[3], l = idx[4])
  }

  return(value[[1]])

}


#' Get Spatial Point from Index
#'
#' Get spatial point from index of an \code{antsImage}.
#'
#'
#' @param x image object of S4 class \code{antsImage} to get values from.
#' @param index image index
#' @return array of pixel values
#' @examples
#'
#' img <- makeImage(c(10,10),rnorm(100))
#' pt <- antsTransformIndexToPhysicalPoint(img, c(2,2))
#'
#'
#' @export antsTransformIndexToPhysicalPoint
antsTransformIndexToPhysicalPoint <- function(x, index) {
  if (class(x)[1] != "antsImage") {
    stop("Input must be of class 'antsImage'")
  }
  if ((class(index) != "numeric") && (class(index) != "matrix")) {
    stop("index must be of class 'numeric' or 'matrix'")
  }

  if (class(index) == "numeric") {
    index <- t(as.matrix(index))
  }

  imgdim <- length(dim(x))
  if (dim(index)[2] != imgdim) {
    stop(paste("Index matrix must be of size N x", imgdim))
  }

  return(.Call("antsImage_TransformIndexToPhysicalPoint", x, index, PACKAGE = "ANTsR"))
}



#' Get Index from Spatial Point
#'
#' Get index from spatial point of an 'antsImage'.
#'
#'
#' @param x Image object of S4 class 'antsImage' to get values from.
#' @param point image physical point
#' @return array of pixel values
#' @examples
#'
#' img<-makeImage(c(10,10),rnorm(100))
#' pt<-antsTransformPhysicalPointToIndex(img,c(2,2))
#'
#'
#' @export antsTransformPhysicalPointToIndex
antsTransformPhysicalPointToIndex <- function(x, point) {
  if (class(x)[1] != "antsImage") {
    stop("Input must be of class 'antsImage'")
  }
  if ((class(point) != "numeric") && (class(point) != "matrix")) {
    stop("point must be of class 'numeric' or 'matrix'")
  }

  if (class(point) == "numeric") {
    point <- t(as.matrix(point))
  }

  imgdim <- length(dim(x))
  if (dim(point)[2] != imgdim) {
    stop(paste("Point matrix must be of size N x", imgdim))
  }

  return(.Call("antsImage_TransformPhysicalPointToIndex", x, point, PACKAGE = "ANTsR"))
}


#' Set a pixel value at an index
#'
#' Set a pixel value at an index in an 'antsImage'.
#'
#'
#' @param x Image object of S4 class 'antsImage'.
#' @param i the slowest moving index to the image
#' @param j the next slowest moving index to the image, similar for k ( 2d )
#' @param k the next slowest moving index to the image ( 3d )
#' @param l the next slowest moving index to the image ( 4d )
#' @param value the value to place at this location
#' @return array of pixel values
#' @examples
#'
#' img<-makeImage(c(10,10),rnorm(100))
#' antsSetPixels(img,2,3,value=Inf)
#'
#'
#' @export antsTransformPhysicalPointToIndex
antsSetPixels <- function(x, i = NA, j = NA, k = NA, l = NA, value) {
  lst <- NULL
  if (length(i) != 1 || !is.na(i)) {
    if (is.null(i)) {
      lst <- c(lst, list(integer(0)))
    } else if (class(i) == "integer" || class(i) == "numeric") {
      lst <- c(lst, list(i))
    } else {
      stop("indices must be of class 'integer' or 'numeric'")
    }
  }

  if (length(j) != 1 || !is.na(j)) {
    if (is.null(j)) {
      lst <- c(lst, list(integer(0)))
    } else if (class(j) == "integer" || class(j) == "numeric") {
      lst <- c(lst, list(j))
    } else {
      stop("indices must be of class 'integer' or 'numeric'")
    }
  }

  if (length(k) != 1 || !is.na(k)) {
    if (is.null(k)) {
      lst <- c(lst, list(integer(0)))
    } else if (class(k) == "integer" || class(k) == "numeric") {
      lst <- c(lst, list(k))
    } else {
      stop("indices must be of class 'integer' or 'numeric'")
    }
  }

  if (length(l) != 1 || !is.na(l)) {
    if (is.null(l)) {
      lst <- c(lst, list(integer(0)))
    } else if (class(l) == "integer" || class(l) == "numeric") {
      lst <- c(lst, list(l))
    } else {
      stop("indices must be of class 'integer' or 'numeric'")
    }
  }
  returnList <- (.Call("antsImage_SetPixels", x, lst, value, PACKAGE = "ANTsR"))

  if ( returnList$flag > 0 ) {
    warning( returnList$error )
  }
  return( returnList$image )
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
  components = FALSE, reference = NA, ...) {
  standardGeneric("as.antsImage")
})

#' @rdname as.antsImage
#' @aliases as.antsImage,matrix-method
setMethod(f = "as.antsImage", signature(object = "matrix"), definition = function(object,
  pixeltype = "float", spacing = as.numeric(seq.int(from = 1, by = 0, length.out = length(dim(object)))),
  origin = as.numeric(seq.int(from = 0, by = 0, length.out = length(dim(object)))),
  direction = diag(length(dim(object))), components = FALSE, reference = NA) {
  if ( is.antsImage(reference) )
    {
    pixeltype = reference@pixeltype
    components = (reference@components > 1)
    spacing = antsGetSpacing(reference)
    origin = antsGetOrigin(reference)
    direction = antsGetDirection(reference)
    }
  return(.Call("antsImage_asantsImage", object, pixeltype, spacing, origin, direction, components, PACKAGE = "ANTsR"))
})

#' @rdname as.antsImage
#' @aliases as.antsImage,array-method
setMethod(f = "as.antsImage", signature(object = "array"), definition = function(object,
  pixeltype = "float", spacing = as.numeric(seq.int(from = 1, by = 0, length.out = length(dim(object)))),
  origin = as.numeric(seq.int(from = 0, by = 0, length.out = length(dim(object)))),
  direction = diag(length(dim(object))), components = FALSE, reference = NA) {
  if ( is.antsImage(reference) )
    {
    pixeltype = reference@pixeltype
    components = (reference@components > 1)
    spacing = antsGetSpacing(reference)
    origin = antsGetOrigin(reference)
    direction = antsGetDirection(reference)
    }
  return(.Call("antsImage_asantsImage", object, pixeltype, spacing, origin, direction, components, PACKAGE = "ANTsR"))
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

# #' @title arith.antsImage
# #' @description Atomic arithmetic operators for antsImages
# #' @param x antsImage
# #' @param y antsImage or numeric
# #' @param mask antsImage logical mask (optional)
# #' @examples
# #' r16 <- antsImageRead(getANTsRData('r16'), 2)
# #' r64 <- antsImageRead(getANTsRData('r64'), 2)
# #' r16 + r64
# #' r16 + 5
# #' r16 / 10
# #' @name antsImageArith
# "+.antsImage" <- function(x, y){
#   if(is.antsImage(y)){
#     if ( !antsImagePhysicalSpaceConsistency(x,y) ){
#       stop("Images do not occupy the same physical space")
#     }
#     imgsum <- as.antsImage(as.array(x) + as.array(y), reference=x)
#   } else{
#     imgsum <- as.antsImage(as.array(x) + y, reference=x)
#   }
#   antsCopyImageInfo(x, imgsum)
# }
# 
# #' @rdname antsImageArith
# "-.antsImage" <- function(x, y){
#   if(missing(y)) {
#     imgdif <- as.antsImage(0 - as.array(x), reference=x)
#   } else if(is.antsImage(y)){
#     if ( !antsImagePhysicalSpaceConsistency(x,y) ){
#       stop("Images do not occupy the same physical space")
#     }
#     imgdif <- as.antsImage(as.array(x) - as.array(y), reference=x)
#   } else {
#     imgdif <- as.antsImage(as.array(x) - y, reference=x)
#   }
#   antsCopyImageInfo(x, imgdif)
# }
# 
# #' @rdname antsImageArith
# "/.antsImage" <- function(x, y){
#   if(is.antsImage(y)){
#     if ( !antsImagePhysicalSpaceConsistency(x,y) ){
#       stop("Images do not occupy the same physical space")
#     }
#     imgfrac <- as.antsImage(as.array(x) / as.array(y), reference=x)
#   } else{
#     imgfrac <- as.antsImage(as.array(x) / y, reference=x)
#   }
#   antsCopyImageInfo(x, imgfrac)
# }
# 
# #' @rdname antsImageArith
# "*.antsImage" <- function(x, y){
#   if(is.antsImage(y)){
#     if ( !antsImagePhysicalSpaceConsistency(x,y) ){
#       stop("Images do not occupy the same physical space")
#     }
#     imgmult <- as.antsImage(as.array(x) * as.array(y), reference=x)
#   } else{
#     imgmult <- as.antsImage(as.array(x) * y, reference=x)
#   }
#   antsCopyImageInfo(x, imgmult)
# }
# 
# #' @rdname antsImageArith
# "^.antsImage" <- function(x, y){
#   if(is.antsImage(y)){
#     if ( !antsImagePhysicalSpaceConsistency(x,y) ){
#       stop("Images do not occupy the same physical space")
#     }
#     imgpow <- as.antsImage(as.array(x) ^ as.array(y), reference=x)
#   } else{
#     imgpow <- as.antsImage(as.array(x) ^ y, reference=x)
#   }
#   antsCopyImageInfo(x, imgpow)
# }
# 
# #' @rdname antsImageArith
# "%%.antsImage" <- function(x, y){
#   if(is.antsImage(y)){
#     if ( !antsImagePhysicalSpaceConsistency(x,y) ){
#       stop("Images do not occupy the same physical space")
#     }
#     imgmod <- as.antsImage(as.array(x) %% as.array(y), reference=x)
#   } else {
#     imgmod <- as.antsImage(as.array(x) %% y, reference=x)
#   }
#   antsCopyImageInfo(x, imgmod)
# }
# 
# #' @param ... Additional arguments passed to underlying R operator
# #' @examples
# #' log(r16, base=10)
# #' @rdname antsImageArith
# "log.antsImage" <- function(x, ...){
#   imglog <- as.antsImage(log(as.array(x), ...), reference=x)
#   antsCopyImageInfo(x, imglog)
# }
# 
# #' @rdname antsImageArith
# "exp.antsImage" <- function(x){
#   imgexp <- as.antsImage(exp(as.array(x)), reference=x)
#   antsCopyImageInfo(x, imgexp)
# }
# 
# #' @rdname antsImageArith
# "abs.antsImage" <- function(x){
#   imgabs <- as.antsImage(abs(as.array(x)), reference=x)
#   antsCopyImageInfo(x, imgabs)
# }
