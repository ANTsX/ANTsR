# this file defines the S4 classes related to 'antsImage' and their associated
# methods

#' An S4 class to hold a region of an antsImage
#'
#' @slot index
#' @slot size
#' @useDynLib ANTsR
#' @import Rcpp
#' @import methods
#' @import tools
setClass(Class = "antsRegion", representation(index = "numeric", size = "numeric"))


#' An S4 class for an antsImage
#'
#' C++ type used to represent an ITK image pointed to by 'pointer'. the
#' actual image is of C++ type 'itk::image< pixeltype , dimension >::Pointer'
#'
#' @param object input object to convert
#' @param .Object input object to convert
#' @param pixeltype string e.g. "float" "unsigned char" "int"
#' @param dimension dimensionality of the image
#' @param components number of components per pixel
#' @param x input object to convert
#' @param mask mask for the region
#' @param region antsRegion for the image
#' @param e1 internal control for types
#' @param e2 internal control for types
#' @slot pixeltype usually float, can be other types unsigned char, int, double
#' etc noting that short is not supported
#' @slot dimension usually 2 or 3 but can be 4
#' @slot components number of pixel components
#' @slot pointer the memory location
setClass(Class = "antsImage", representation(pixeltype = "character", dimension = "integer",
  components = "integer", pointer = "externalptr"))

#' @describeIn antsImage
setMethod(f = "show", "antsImage", function(object){
    cat("antsImage\n")
    cat("  Pixel Type   :", object@pixeltype, "\n")
    cat("  Pixel Size   :", object@components, "\n")
    cat("  Dimensions   :", paste(dim(object), collapse="x"), "\n")
    cat("  Voxel Spacing:", paste(antsGetSpacing(object), collapse="x"), "\n")
    cat("  Origin       :", antsGetOrigin(object), "\n")
    cat("  Direction    :", antsGetDirection(object), "\n")
    cat("\n")
})
#' @describeIn antsImage
setMethod(f = "initialize", signature(.Object = "antsImage"), definition = function(.Object,
  pixeltype = "float", dimension = 3, components = 1) {
  return(.Call("antsImage", pixeltype, dimension, components, PACKAGE = "ANTsR"))
})

#' @describeIn antsImage
setMethod(f = "dim", signature(x = "antsImage"), definition = function(x) {
  return(.Call("antsImage_dim", x, PACKAGE = "ANTsR"))
})

#' @describeIn antsImage
setMethod(f = "min", signature(x = "antsImage"), definition = function(x) {
  return(min(as.array(x)))
})

#' @describeIn antsImage
setMethod(f = "max", signature(x = "antsImage"), definition = function(x) {
  return(max(as.array(x)))
})

#' @describeIn antsImage
setMethod(f = "var", signature(x = "antsImage"), definition = function(x) {
  return(var(as.array(x)))
})

#' @describeIn antsImage
setMethod(f = "sd", signature(x = "antsImage"), definition = function(x) {
  return(sd(as.array(x)))
})

###  @  describeIn antsImage # this doesnt work b/c nnz is not a generic
# setGeneric("nnz",function(x){standardGeneric("nnz")})
# setMethod(f = "nnz", signature(x = "antsImage"), definition = function(x) {
#   return(length( which( as.array(x) != 0)))
# })

#' @rdname antsImageArith
setMethod(f = "mean", signature(x = "antsImage"), definition = function(x, mask = logical()) {
  if (typeof(mask) != "logical") {
    stop("'mask' provided is not of type 'logical'")
  }

  if (length(mask) == 0) {
    return(mean(as.array(x)))
  } else {
    return(mean(as.array(x)[mask]))
  }

})

#' @describeIn antsImage
setMethod(f = "is.na", signature(x = "antsImage"), definition = function(x) {
  val <- .Call("antsImage_isna", x, PACKAGE = "ANTsR")
  if (val > 0) {
    return(TRUE)
  }
  return(FALSE)
})

#' @describeIn antsImage
setMethod(f = "as.numeric", signature(x = "antsImage"), definition = function(x,
  mask = logical(), region = new("antsRegion", index = integer(), size = integer())) {
  if (typeof(mask) != "logical") {
    stop("'mask' provided is not of type 'logical'")
  }
  return(.Call("antsImage_asVector", x, mask, region, PACKAGE = "ANTsR"))
})

#' @describeIn antsImage
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

#' @describeIn antsImage
#' @export as.array.antsImage
setMethod(f = "as.array", signature(x = "antsImage"),
 definition = function(x, mask = logical(),
  region = new("antsRegion", index = integer(), size = integer())) {
  if (typeof(mask) != "logical") {
    stop("'mask' provided is not of type 'logical'")
  }
  return(.Call("antsImage_asVector", x, mask, region, PACKAGE = "ANTsR"))
})

# see https://github.com/klutometis/roxygen/issues/272

#' @param x antsImage
#' @param i logical or i-th dimension
#' @param j not used or j-th dimension
#' @param drop method for missing data
#' @param k not used or k-th dimension
#' @param l not used or l-th dimension
#' @param value ok
#' @describeIn as.antsImage
setMethod("[", c( "antsImage", "NULL", "ANY", "ANY"),
  definition = function(x, i, j,..., drop) {
  mask <- logical(0)
  region <- new("antsRegion", index = integer(), size = integer())
  return(.Call("antsImage_asVector", x, mask, region, PACKAGE = "ANTsR"))
})

#' @describeIn as.antsImage
setMethod("[", c( "antsImage", "NULL", "ANY"),
  definition = function(x, i, j,..., drop) {
  mask <- logical(0)
  region <- new("antsRegion", index = integer(), size = integer())
  return(.Call("antsImage_asVector", x, mask, region, PACKAGE = "ANTsR"))
})


#' @describeIn as.antsImage
setMethod(f = "[", signature(x = "antsImage", i = "logical", j="ANY", "ANY"),
  definition = function(x, i, j, ..., drop) {
  region <- new("antsRegion", index = integer(), size = integer())
  return(.Call("antsImage_asVector", x, i, region, PACKAGE = "ANTsR"))
})

#' @describeIn as.antsImage
setMethod(f = "[", signature(x = "antsImage", i = "logical", j="ANY"),
  definition = function(x, i, j, ..., drop) {
  region <- new("antsRegion", index = integer(), size = integer())
  return(.Call("antsImage_asVector", x, i, region, PACKAGE = "ANTsR"))
})


#' @describeIn as.antsImage
setMethod(f = "[", signature(x = "antsImage", i = "ANY", j="ANY", "ANY"),
  definition = function(x, i, j, ..., drop) {
  if (typeof(i) != "logical") {
    stop("'mask' provided is not of type 'logical'")
  }
  region <- new("antsRegion", index = integer(), size = integer())
  return(.Call("antsImage_asVector", x, i, region, PACKAGE = "ANTsR"))
})

#' @describeIn as.antsImage
setMethod(f = "[", signature(x = "antsImage", i = "ANY", j="ANY"),
  definition = function(x, i, j, ..., drop) {
  if (typeof(i) != "logical") {
    stop("'mask' provided is not of type 'logical'")
  }
  region <- new("antsRegion", index = integer(), size = integer())
  return(.Call("antsImage_asVector", x, i, region, PACKAGE = "ANTsR"))
})


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
#' @param radius array of values for neighborhood radius (in voxels)
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
#' nhlist<-getNeighborhoodAtVoxel(img,center,radius)
#'
#'
#' @export getNeighborhoodAtVoxel
getNeighborhoodAtVoxel <- function(image, center, radius, physical.coordinates = FALSE) {

  if (class(image)[1] != "antsImage") {
    stop("Input must be of class 'antsImage'")
  }

  if ((class(center) != "numeric")) {
    stop("center must be of class 'numeric'")
  }

  if ((class(radius) != "numeric")) {
    stop("radius must be of class 'numeric'")
  }

  return(.Call("antsImage_GetNeighborhood", image, center, radius, physical.coordinates))
}






#' @name getNeighborhoodInMask
#' @title Get neighborhoods for voxels within mask
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

#' @describeIn as.antsImage
setMethod(f = "[", signature(x = "antsImage", i = "NULL", j = "NULL", "ANY"),
 definition = function(x, i, j, k = NA, l = NA, ..., drop ) {
  return(getPixels(x, i, j, k, l))
})

#' @describeIn as.antsImage
setMethod(f = "[", signature(x = "antsImage", i = "NULL", j = "NULL"),
 definition = function(x, i, j, k = NA, l = NA, ..., drop ) {
  return(getPixels(x, i, j, k, l))
})

#' @describeIn as.antsImage
setMethod("[", signature(x = "antsImage", i = "numeric", j = "numeric", "ANY"),
 definition = function(x, i, j, k = NA, l = NA, ..., drop) {
  return(getPixels(x, i, j, k, l))
})

#' @describeIn as.antsImage
setMethod("[", signature(x = "antsImage", i = "numeric", j = "numeric"),
 definition = function(x, i, j, k = NA, l = NA, ..., drop) {
  return(getPixels(x, i, j, k, l))
})

#' @describeIn as.antsImage
setMethod(f = "[", signature(x = "antsImage", i = "numeric", j = "NULL", "ANY"),
 definition = function(x, i, j, k = NA, l = NA, ..., drop) {
  return(getPixels(x, i, j, k, l))
})

#' @describeIn as.antsImage
setMethod(f = "[", signature(x = "antsImage", i = "numeric", j = "NULL"),
 definition = function(x, i, j, k = NA, l = NA, ..., drop) {
  return(getPixels(x, i, j, k, l))
})

#' @describeIn as.antsImage
setMethod(f = "[", signature(x = "antsImage", i = "NULL", j = "numeric", "ANY"),
 definition = function(x, i, j, k = NA, l = NA, ..., drop) {
  return(getPixels(x, i, j, k, l))
})

#' @describeIn as.antsImage
setMethod(f = "[", signature(x = "antsImage", i = "NULL", j = "numeric"),
 definition = function(x, i, j, k = NA, l = NA, ..., drop) {
  return(getPixels(x, i, j, k, l))
})

# > getGeneric("[<-")
# standardGeneric for "[<-" defined from package "base"
# function (x, i, j, ..., value)

#' @describeIn as.antsImage
setMethod(f = "[<-", signature(x = "antsImage", i = "NULL"),
  definition = function(x, i, j, ..., value) {
  mask <- logical(0)
  region <- new("antsRegion", index = integer(), size = integer())
  return(.Call("antsImage_SetRegion", x, mask, region, value, PACKAGE = "ANTsR"))
})

#' @describeIn as.antsImage
setMethod(f = "[<-", signature(x = "antsImage", i = "logical"),
  definition = function(x, i, j, ..., value) {
  region <- new("antsRegion", index = integer(), size = integer())
  return(.Call("antsImage_SetRegion", x, i, region, value, PACKAGE = "ANTsR"))
})

#' @describeIn as.antsImage
setMethod(f = "[<-", signature(x = "antsImage", i = "array"),
  definition = function(x, i, j, ..., value) {
  if (typeof(i) != "logical") {
    stop("'mask' provided is not of type 'logical'")
  }
  region <- new("antsRegion", index = integer(), size = integer())
  return(.Call("antsImage_SetRegion", x, i, region, value, PACKAGE = "ANTsR"))
})


#' @describeIn as.antsImage
setMethod(f = "[<-", signature(x = "antsImage", i = "matrix"),
  definition = function(x, i, j, ..., value) {
  if (typeof(i) != "logical") {
    stop("'mask' provided is not of type 'logical'")
  }
  region <- new("antsRegion", index = integer(), size = integer())
  return(.Call("antsImage_SetRegion", x, i, region, value, PACKAGE = "ANTsR"))
})

#' @describeIn as.antsImage
setMethod(f = "[<-", signature(x = "antsImage", i = "list"),
  definition = function(x, i, j, ..., value) {
  if (class(i$mask) == "NULL") {
    i$mask <- logical(0)
  } else if (typeof(i$mask) != "logical") {
    stop("'mask' provided is not of type 'logical'")
  }
  if (class(i$region) != "antsRegion") {
    stop("'region' provided is not of class 'antsRegion'")
  }
  return(.Call("antsImage_SetRegion", x, i$mask, i$region, value, PACKAGE = "ANTsR"))
})

#' @describeIn as.antsImage
setMethod(f = "[<-", signature(x = "antsImage", i = "NULL", j = "antsRegion"), definition = function(x, i, j, ..., value) {
  mask <- logical(0)
  return(.Call("antsImage_SetRegion", x, mask, j, value, PACKAGE = "ANTsR"))
})

#' @describeIn as.antsImage
setMethod(f = "[<-", signature(x = "antsImage", i = "logical", j = "antsRegion"),
  definition = function(x, i, j, ..., value) {
    return(.Call("antsImage_SetRegion", x, i, j, value, PACKAGE = "ANTsR"))
  })


#' @describeIn as.antsImage
setMethod(f = "[<-", signature(x = "antsImage", i = "array", j = "antsRegion"),
definition = function(x, i, j, ..., value) {
  if (typeof(i) != "logical") {
    stop("'mask' provided is not of type 'logical'")
  }
  return(.Call("antsImage_SetRegion", x, i, j, value, PACKAGE = "ANTsR"))
})

#' @describeIn as.antsImage
setMethod(f = "[<-", signature(x = "antsImage", i = "matrix", j = "antsRegion"),
  definition = function(x, i, j, ..., value) {
    if (typeof(i) != "logical") {
      stop("'mask' provided is not of type 'logical'")
    }
    return(.Call("antsImage_SetRegion", x, i, j, value, PACKAGE = "ANTsR"))
  })

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
  temp<-(.Call("antsImage_SetPixels", x, lst, value, PACKAGE = "ANTsR"))
}


#' @describeIn as.antsImage
setMethod(f = "[<-", signature(x = "antsImage", i = "NULL", j = "NULL", value = "numeric"),
  definition = function(x, i, j, ..., value) {
    temp<-antsSetPixels(x, i, j, ..., value = value)
  })

#' @describeIn as.antsImage
setMethod(f = "[<-", signature(x = "antsImage", i = "numeric", j = "numeric", value = "numeric"),
  definition = function(x, i, j, ..., value) {
    temp<-antsSetPixels(x, i, j, ..., value = value)
  })

#' @describeIn as.antsImage
setMethod(f = "[<-", signature(x = "antsImage", i = "numeric", j = "NULL", value = "numeric"),
  definition = function(x, i, j, ..., value) {
    temp<-antsSetPixels(x, i, j, ..., value = value)
  })


#' @describeIn as.antsImage
setMethod(f = "[<-", signature(x = "antsImage", i = "NULL", j = "numeric", value = "numeric"),
  definition = function(x, i, j, ..., value) {
    temp<-antsSetPixels(x, i, j, ..., value = value)
  })

#' as.antsImage
#'
#' convert types to antsImage
#'
#' @param object An object
#' @param pixeltype a character string e.g. "float"
#' @param spacing numeric vector matching image dimensionality e.g. c(1.2,1.2)
#' @param origin numeric vector matching image dimensionality e.g. c(0,0)
#' @param ... Extra named arguments passed to FUN
#' @rdname as.antsImage
#' @export
setGeneric(name = "as.antsImage", def = function(object, ...) standardGeneric("as.antsImage"))

#' @describeIn as.antsImage
setMethod(f = "as.antsImage", signature(object = "matrix"), definition = function(object,
  pixeltype = "float", spacing = as.numeric(seq.int(from = 1, by = 0, length.out = length(dim(object)))),
  origin = as.numeric(seq.int(from = 0, by = 0, length.out = length(dim(object))))) {
  return(.Call("antsImage_asantsImage", object, pixeltype, spacing, origin, PACKAGE = "ANTsR"))
})

#' @describeIn as.antsImage
setMethod(f = "as.antsImage", signature(object = "array"), definition = function(object,
  pixeltype = "float", spacing = as.numeric(seq.int(from = 1, by = 0, length.out = length(dim(object)))),
  origin = as.numeric(seq.int(from = 0, by = 0, length.out = length(dim(object))))) {
  return(.Call("antsImage_asantsImage", object, pixeltype, spacing, origin, PACKAGE = "ANTsR"))
})

#' @describeIn antsImage
setMethod(f = "==", signature(e1 = "antsImage"), definition = function(e1, e2) {
  operator <- "=="
  if (class(e2) == "list") {
    if (length(e2$value) != 1) {
      stop("length of value must be 1")
    }
    if (class(e2$region) != "antsRegion") {
      stop("region argument not of class 'antsRegion'")
    }
    return(.Call("antsImage_RelationalOperators", e1, e2$value, e2$region, operator,
      PACKAGE = "ANTsR"))
  } else if ((class(e2) == "numeric" | class(e2) == "integer") && length(e2) == 1) {
    if(class(e2) == "integer")
      e2 <- as.numeric(e2)
    region <- new("antsRegion", index = integer(), size = integer())
    return(.Call("antsImage_RelationalOperators", e1, e2, region,
      operator, PACKAGE = "ANTsR"))
  } else {
    stop("rhs must be a scalar or a list( <scalar> , <antsRegion> )")
  }
})


#' @describeIn antsImage
setMethod(f = "!=", signature(e1 = "antsImage"), definition = function(e1, e2) {
  operator <- "!="
  if (class(e2) == "list") {
    if (length(e2$value) != 1) {
      stop("length of value must be 1")
    }
    if (class(e2$region) != "antsRegion") {
      stop("region argument not of class 'antsRegion'")
    }
    return(.Call("antsImage_RelationalOperators", e1, e2$value, e2$region, operator,
      PACKAGE = "ANTsR"))
  } else if (class(e2) == "numeric" && length(e2) == 1) {
    region <- new("antsRegion", index = integer(), size = integer())
    return(.Call("antsImage_RelationalOperators", e1, e2, region, operator, PACKAGE = "ANTsR"))
  } else {
    stop("rhs must be a scalar or a list( <scalar> , <antsRegion> )")
  }
})

#' @describeIn antsImage
setMethod(f = "<=", signature(e1 = "antsImage"), definition = function(e1, e2) {
  operator <- "<="
  if (class(e2) == "list") {
    if (length(e2$value) != 1) {
      stop("length of value must be 1")
    }
    if (class(e2$region) != "antsRegion") {
      stop("region argument not of class 'antsRegion'")
    }
    return(.Call("antsImage_RelationalOperators", e1, e2$value, e2$region, operator,
      PACKAGE = "ANTsR"))
  } else if (class(e2) == "numeric" && length(e2) == 1) {
    region <- new("antsRegion", index = integer(), size = integer())
    return(.Call("antsImage_RelationalOperators", e1, e2, region, operator, PACKAGE = "ANTsR"))
  } else {
    stop("rhs must be a scalar or a list( <scalar> , <antsRegion> )")
  }
})

#' @describeIn antsImage
setMethod(f = ">=", signature(e1 = "antsImage"), definition = function(e1, e2) {
  operator <- ">="
  if (class(e2) == "list") {
    if (length(e2$value) != 1) {
      stop("length of value must be 1")
    }
    if (class(e2$region) != "antsRegion") {
      stop("region argument not of class 'antsRegion'")
    }
    return(.Call("antsImage_RelationalOperators", e1, e2$value, e2$region, operator,
      PACKAGE = "ANTsR"))
  } else if (class(e2) == "numeric" && length(e2) == 1) {
    region <- new("antsRegion", index = integer(), size = integer())
    return(.Call("antsImage_RelationalOperators", e1, e2, region, operator, PACKAGE = "ANTsR"))
  } else {
    stop("rhs must be a scalar or a list( <scalar> , <antsRegion> )")
  }
})

#' @describeIn antsImage
setMethod(f = "<", signature(e1 = "antsImage"), definition = function(e1, e2) {
  operator <- "<"
  if (class(e2) == "list") {
    if (length(e2$value) != 1) {
      stop("length of value must be 1")
    }
    if (class(e2$region) != "antsRegion") {
      stop("region argument not of class 'antsRegion'")
    }
    return(.Call("antsImage_RelationalOperators", e1, e2$value, e2$region, operator,
      PACKAGE = "ANTsR"))
  } else if (class(e2) == "numeric" && length(e2) == 1) {
    region <- new("antsRegion", index = integer(), size = integer())
    return(.Call("antsImage_RelationalOperators", e1, e2, region, operator, PACKAGE = "ANTsR"))
  } else {
    stop("rhs must be a scalar or a list( <scalar> , <antsRegion> )")
  }
})

#' @describeIn antsImage
setMethod(f = ">", signature(e1 = "antsImage"), definition = function(e1, e2) {
  operator <- ">"
  if (class(e2) == "list") {
    if (length(e2$value) != 1) {
      stop("length of value must be 1")
    }
    if (class(e2$region) != "antsRegion") {
      stop("region argument not of class 'antsRegion'")
    }
    return(.Call("antsImage_RelationalOperators", e1, e2$value, e2$region, operator,
      PACKAGE = "ANTsR"))
  } else if (class(e2) == "numeric" && length(e2) == 1) {
    region <- new("antsRegion", index = integer(), size = integer())
    return(.Call("antsImage_RelationalOperators", e1, e2, region, operator, PACKAGE = "ANTsR"))
  } else {
    stop("rhs must be a scalar or a list( <scalar> , <antsRegion> )")
  }
})

#' @title is.antsImage
#'
#' @description Tests if object is antsImage
#'
#' @param x An object
#' @return TRUE if \code{object} is antsImage; FALSE otherwise.
#' @examples
#' is.antsImage(antsImageRead(getANTsRData('r16'), 2))
#' @export is.antsImage
is.antsImage <- function(x){
  class(x) == 'antsImage'
}

#' @title arith.antsImage
#' @description Atomic arithmetic operators for antsImages
#' @param x antsImage
#' @param y antsImage or numeric
#' @param mask antsImage logical mask (optional)
#' @examples
#' r16 <- antsImageRead(getANTsRData('r16'), 2)
#' r64 <- antsImageRead(getANTsRData('r64'), 2)
#' r16 + r64
#' r16 + 5
#' r16 / 10
#' @name antsImageArith
"+.antsImage" <- function(x, y){
  if(is.antsImage(y)){
    if ( !antsImagePhysicalSpaceConsistency(x,y) ){
      stop("Images do not occupy the same physical space")
    }
    imgsum <- as.antsImage(as.array(x) + as.array(y))
  } else{
    imgsum <- as.antsImage(as.array(x) + y)
  }
  antsCopyImageInfo(x, imgsum)
}

#' @rdname antsImageArith
"-.antsImage" <- function(x, y){
  if(is.antsImage(y)){
    if ( !antsImagePhysicalSpaceConsistency(x,y) ){
      stop("Images do not occupy the same physical space")
    }
    imgdif <- as.antsImage(as.array(x) - as.array(y))
  } else{
    imgdif <- as.antsImage(as.array(x) - y)
  }
  antsCopyImageInfo(x, imgdif)
}

#' @rdname antsImageArith
"/.antsImage" <- function(x, y){
  if(is.antsImage(y)){
    if ( !antsImagePhysicalSpaceConsistency(x,y) ){
      stop("Images do not occupy the same physical space")
    }
    imgfrac <- as.antsImage(as.array(x) / as.array(y))
  } else{
    imgfrac <- as.antsImage(as.array(x) / y)
  }
  antsCopyImageInfo(x, imgfrac)
}

#' @rdname antsImageArith
"*.antsImage" <- function(x, y){
  if(is.antsImage(y)){
    if ( !antsImagePhysicalSpaceConsistency(x,y) ){
      stop("Images do not occupy the same physical space")
    }
    imgmult <- as.antsImage(as.array(x) * as.array(y))
  } else{
    imgmult <- as.antsImage(as.array(x) * y)
  }
  antsCopyImageInfo(x, imgmult)
}

#' @rdname antsImageArith
"^.antsImage" <- function(x, y){

  if(is.antsImage(y)){
    if ( !antsImagePhysicalSpaceConsistency(x,y) ){
      stop("Images do not occupy the same physical space")
    }
    imgpow <- as.antsImage(as.array(x) ^ as.array(y))
  } else{
    imgpow <- as.antsImage(as.array(x) ^ y)
  }
  antsCopyImageInfo(x, imgpow)
}

#' @rdname antsImageArith
"%%.antsImage" <- function(x, y){
  if(is.antsImage(y)){
    if ( !antsImagePhysicalSpaceConsistency(x,y) ){
      stop("Images do not occupy the same physical space")
    }
    imgmod <- as.antsImage(as.array(x) %% as.array(y))
  } else {
    imgmod <- as.antsImage(as.array(x) %% y)
  }
  antsCopyImageInfo(x, imgmod)
}

#' @param ... Additional arguments passed to underlying R operator
#' @examples
#' log(r16, base=10)
#' @rdname antsImageArith
"log.antsImage" <- function(x, ...){
  imglog <- as.antsImage(log(as.array(x), ...))
  antsCopyImageInfo(x, imglog)
}

#' @rdname antsImageArith
"exp.antsImage" <- function(x){
  imgexp <- as.antsImage(exp(as.array(x)))
  antsCopyImageInfo(x, imgexp)
}
