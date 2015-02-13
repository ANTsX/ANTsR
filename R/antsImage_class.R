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
#' @param x input object to convert
#' @param mask mask for the region
#' @param region antsRegion for the image
#' @param e1 internal control for types
#' @param e2 internal control for types
#' @slot pixeltype usually float, can be other types unsigned char, int, double
#' etc noting that short is not supported
#' @slot dimension usually 2 or 3 but can be 4
#' @slot pointer the memory location
setClass(Class = "antsImage", representation(pixeltype = "character", dimension = "integer",
  pointer = "externalptr"))

#' @describeIn antsImage
setMethod(f = "show", "antsImage", function(object){
    cat("antsImage\n")
    cat("  Pixel Type   :", object@pixeltype, "\n")
    cat("  Dimensions   :", paste(dim(object), collapse="x"), "\n")
    cat("  Voxel Spacing:", paste(antsGetSpacing(object), collapse="x"), "\n")
    cat("  Origin       :", antsGetOrigin(object), "\n")
    cat("  Direction    :", antsGetDirection(object), "\n")
    cat("\n")
})
#' @describeIn antsImage
setMethod(f = "initialize", signature(.Object = "antsImage"), definition = function(.Object,
  pixeltype = "float", dimension = 3) {
  return(.Call("antsImage", pixeltype, dimension, PACKAGE = "ANTsR"))
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

#' @describeIn antsImage
setMethod(f = "mean", signature(x = "antsImage"), definition = function(x, mask = logical()) {
  if (typeof(mask) != "logical") {
    print("'mask' provided is not of type 'logical'")
    return()
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
    print("'mask' provided is not of type 'logical'")
    return()
  }
  return(.Call("antsImage_asVector", x, mask, region, PACKAGE = "ANTsR"))
})

#' @describeIn antsImage
setMethod(f = "as.matrix", signature(x = "antsImage"),
 definition = function(x, mask = logical(),
  region = new("antsRegion", index = integer(), size = integer())) {
  if (typeof(mask) != "logical") {
    print("'mask' provided is not of type 'logical'")
    return()
  }
  if (x@dimension != 2) {
    print("image dimension must be 2")
    return()
  }
  return(.Call("antsImage_asVector", x, mask, region, PACKAGE = "ANTsR"))
})

#' @describeIn antsImage
#' @export as.array.antsImage
setMethod(f = "as.array", signature(x = "antsImage"),
 definition = function(x, mask = logical(),
  region = new("antsRegion", index = integer(), size = integer())) {
  if (typeof(mask) != "logical") {
    print("'mask' provided is not of type 'logical'")
    return()
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
    print("'mask' provided is not of type 'logical'")
    return()
  }
  region <- new("antsRegion", index = integer(), size = integer())
  return(.Call("antsImage_asVector", x, i, region, PACKAGE = "ANTsR"))
})

#' @describeIn as.antsImage
setMethod(f = "[", signature(x = "antsImage", i = "ANY", j="ANY"),
  definition = function(x, i, j, ..., drop) {
  if (typeof(i) != "logical") {
    print("'mask' provided is not of type 'logical'")
    return()
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
#' pixel<-antsGetPixels(img,i=c(1,2),j=1)
#'
#'
#' @export antsGetPixels
antsGetPixels <- function(x, i = NA, j = NA, k = NA, l = NA) {
  lst <- NULL
  if (length(i) != 1 || !is.na(i)) {
    if (is.null(i)) {
      lst <- c(lst, list(integer(0)))
    } else if (class(i) == "integer" || class(i) == "numeric") {
      lst <- c(lst, list(i))
    } else {
      print("indices must be of class 'integer' or 'numeric'")
      return()
    }
  }

  if (length(j) != 1 || !is.na(j)) {
    if (is.null(j)) {
      lst <- c(lst, list(integer(0)))
    } else if (class(j) == "integer" || class(j) == "numeric") {
      lst <- c(lst, list(j))
    } else {
      print("indices must be of class 'integer' or 'numeric'")
      return()
    }
  }

  if (length(k) != 1 || !is.na(k)) {
    if (is.null(k)) {
      lst <- c(lst, list(integer(0)))
    } else if (class(k) == "integer" || class(k) == "numeric") {
      lst <- c(lst, list(k))
    } else {
      print("indices must be of class 'integer' or 'numeric'")
      return()
    }
  }

  if (length(l) != 1 || !is.na(l)) {
    if (is.null(l)) {
      lst <- c(lst, list(integer(0)))
    } else if (class(l) == "integer" || class(l) == "numeric") {
      lst <- c(lst, list(l))
    } else {
      print("indices must be of class 'integer' or 'numeric'")
      return()
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
#' @param x Image to access
#' @return For \code{get} methods, vector of size \code{dim(x)}.
#' For \code{set} methods, 0 to indicate success.
antsGetSpacing <- function(x) {
  if (class(x)[1] != "antsImage") {
    print("Input must be of class 'antsImage'")
    return()
  }

  return(.Call("antsImage_GetSpacing", x, PACKAGE = "ANTsR"))
}
#' @rdname antsImageGetSet
#' @param spacing Desired spacing as vector of size dim(x)
antsSetSpacing <- function(x, spacing) {
  if (class(x)[1] != "antsImage") {
    print("Input must be of class 'antsImage'")
    return()
  }

  if ((class(spacing) != "numeric") && (class(spacing) != "array")) {
    print("spacing must be of class 'numeric'")
    return()
  }

  if (length(spacing) != length(dim(x))) {
    print("spacing must be of same dimensions as image")
    return()
  }

  return(.Call("antsImage_SetSpacing", x, spacing, PACKAGE = "ANTsR"))
}

#' @rdname antsImageGetSet
#' @usage antsGetOrigin(x)
antsGetOrigin <- function(x) {
  if (class(x)[1] != "antsImage") {
    print("Input must be of class 'antsImage'")
    return()
  }
  return(.Call("antsImage_GetOrigin", x, PACKAGE = "ANTsR"))
}
#' @rdname antsImageGetSet
#' @usage antsSetOrigin(x, origin)
#' @param origin Desired origin as vector of size dim(x)
antsSetOrigin <- function(x, origin) {
  if (class(x)[1] != "antsImage") {
    print("Input must be of class 'antsImage'")
    return()
  }
  if ((class(origin) != "numeric") && (class(origin) != "array")) {
    print("spacing must be of class 'numeric' or 'array'")
    return()
  }

  if (length(origin) != length(dim(x))) {
    print("spacing must be of same dimensions as image")
    return()
  }

  return(.Call("antsImage_SetOrigin", x, origin, PACKAGE = "ANTsR"))
}

#' @rdname antsImageGetSet
#' @usage antsGetDirection(x)
antsGetDirection <- function(x) {
  if (class(x)[1] != "antsImage") {
    print("Input must be of class 'antsImage'")
    return()
  }
  return(.Call("antsImage_GetDirection", x, PACKAGE = "ANTsR"))
}

#' @rdname antsImageGetSet
#' @usage antsSetDirection(x, direction)
#' @param direction Desired direction as vector of size dim(x)
antsSetDirection <- function(x, direction) {
  if (class(x)[1] != "antsImage") {
    print("Input must be of class 'antsImage'")
    return()
  }
  if ((class(direction) != "matrix") & (class(direction) != "array")) {
    print("direction must be of class 'matrix' or 'array'")
    return()
  }
  if ((dim(direction)[1] != length(dim(x))) || (dim(direction)[2] != length(dim(x)))) {
    print("direction matrix must be of size imagedim * imagedim")
    return()
  }
  return(.Call("antsImage_SetDirection", x, direction, PACKAGE = "ANTsR"))
}


#' Get Neighborhood
#'
#' Get the values in a local neighborhood of an 'antsImage'.
#'
#'
#' @param image Image object of S4 class 'antsImage' to get values from.
#' @param center array of indices for neighborhood center
#' @param radius array of values for neighborhood radius (in voxels)
#' @param physical.coordinates a logical indicating if voxel indices and
#' offsets should be in voxel or physical coordinates
#' @return list containing three matrices: values: matrix of pixel values where
#' the number of rows is the size of the neighborhood and there is a column for
#' each voxel indices: matrix providing the coordinates for each value
#' @author Duda JT
#' @examples
#'
#' img<-makeImage(c(10,10),rnorm(100))
#' center <- dim(img)/2
#' radius <- rep(3,2)
#' mat<-antsGetNeighborhood(img,center,radius)
#'
#'
#' @export antsGetNeighborhood
antsGetNeighborhood <- function(image, center, radius, physical.coordinates = FALSE) {

  if (class(image)[1] != "antsImage") {
    print("Input must be of class 'antsImage'")
    return()
  }

  if ((class(center) != "numeric")) {
    print("center must be of class 'numeric'")
    return()
  }

  if ((class(radius) != "numeric")) {
    print("radius must be of class 'numeric'")
    return()
  }

  return(.Call("antsImage_GetNeighborhood", image, center, radius, physical.coordinates))
}







#' antsGetNeighborhoodMatrix for the masked image
#'
#' Summarize neighborhoods for voxels in an antsImage.
#'
#'
#' @param image Image object of S4 class 'antsImage' to get values from.
#' @param mask Image object of S4 class 'antsImage' indicating which voxels to
#' examine. Each voxel > 0 will be used as the center of a neighborhood
#' @param radius array of values for neighborhood radius (in voxels)
#' @param physical.coordinates a logical indicating if voxel indices and
#' offsets should be in voxel or physical coordinates
#' @param boundary.condition a string indicating how to handle voxels in a
#' neighborhood, but not in the mask 'NA' - fill value with NA 'image' - use
#' image value even if not in mask 'mean' - use mean of all non-NA values for
#' that neighborhood
#' @param spatial.info a boolean indicating of voxel locations and neighborhood
#' offsets should be returned along with pixel values.
#' @return
#'
#' if spatial.info is false: a matrix of pixel values where the number of rows
#' is the size of the neighborhood and there is a column for each voxel
#'
#' if spatial.info is true, a list containing three matrices: values: matrix of
#' pixel values where the number of rows is the size of the neighborhood and
#' there is a column for each voxel indices: matrix providing the center
#' coordinates for each neighborhood offsets: matrix providing the offsets from
#' center for each voxel in a neighborhood
#' @author Duda JT
#' @examples
#'
#' r16<-getANTsRData("r16")
#' r16<-antsImageRead(r16,2)
#' mask<-getMask(r16,lowThresh=mean(r16),cleanup=1)
#' radius <- rep(2,2)
#' mat<-antsGetNeighborhoodMatrix(r16,mask,radius)
#'
#'
#' @export antsGetNeighborhoodMatrix
antsGetNeighborhoodMatrix <- function(image, mask, radius, physical.coordinates = FALSE,
  boundary.condition = "NA", spatial.info = FALSE) {

  if (class(image)[1] != "antsImage") {
    print("Input must be of class 'antsImage'")
    return()
  }

  if ((class(mask) != "antsImage")) {
    print("center must be of class 'antsImage'")
    return()
  }

  if ((class(radius) != "numeric")) {
    print("radius must be of class 'numeric'")
    return()
  }

  if ((prod(radius * 2 + 1) * sum(as.array(mask))) > (2^31 - 1)) {
    print("Requested matrix size is too large for Rcpp")
    return(NA)
  }

  boundary = 0
  if (boundary.condition == "image") {
    boundary = 1
  }
  if (boundary.condition == "mean") {
    boundary = 2
  }

  return(.Call("antsImage_GetNeighborhoodMatrix", image, mask, radius, physical.coordinates,
    boundary, spatial.info))

}

.getValueAtPoint <- function(x, point) {
  if (class(x)[1] != "antsImage") {
    print("Input must be of class 'antsImage'")
    return()
  }
  if ((class(point) != "numeric")) {
    print("point must be of class 'numeric'")
    return()
  }

  idx <- as.numeric(antsTransformPhysicalPointToIndex(x, point))
  idx <- floor(idx)

  dims <- length(idx)

  value <- NA
  if (dims == 2) {
    value <- antsGetPixels(x, i = idx[1], j = idx[2])
  } else if (dims == 3) {
    value <- antsGetPixels(x, i = idx[1], j = idx[2], k = idx[3])
  } else if (dims == 4) {
    value <- antsGetPixels(x, i = idx[1], j = idx[2], k = idx[3], l = idx[4])
  }

  return(value[[1]])

}


#' Get Spatial Point from Index
#'
#' Get spatial point from index of an 'antsImage'.
#'
#'
#' @param x Image object of S4 class 'antsImage' to get values from.
#' @param index image index
#' @return array of pixel values
#' @examples
#'
#' img<-makeImage(c(10,10),rnorm(100))
#' pt<-antsTransformIndexToPhysicalPoint(img,c(2,2))
#'
#'
#' @export antsTransformIndexToPhysicalPoint
antsTransformIndexToPhysicalPoint <- function(x, index) {
  if (class(x)[1] != "antsImage") {
    print("Input must be of class 'antsImage'")
    return()
  }
  if ((class(index) != "numeric") && (class(index) != "matrix")) {
    print("index must be of class 'numeric' or 'matrix'")
    return()
  }

  if (class(index) == "numeric") {
    index <- t(as.matrix(index))
  }

  imgdim <- length(dim(x))
  if (dim(index)[2] != imgdim) {
    print(paste("Index matrix must be of size N x", imgdim))
    return()
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
    print("Input must be of class 'antsImage'")
    return()
  }
  if ((class(point) != "numeric") && (class(point) != "matrix")) {
    print("point must be of class 'numeric' or 'matrix'")
    return()
  }

  if (class(point) == "numeric") {
    point <- t(as.matrix(point))
  }

  imgdim <- length(dim(x))
  if (dim(point)[2] != imgdim) {
    print(paste("Point matrix must be of size N x", imgdim))
    return()
  }

  return(.Call("antsImage_TransformPhysicalPointToIndex", x, point, PACKAGE = "ANTsR"))
}

#' @describeIn as.antsImage
setMethod(f = "[", signature(x = "antsImage", i = "NULL", j = "NULL", "ANY"),
 definition = function(x, i, j, k = NA, l = NA, ..., drop ) {
  return(antsGetPixels(x, i, j, k, l))
})

#' @describeIn as.antsImage
setMethod(f = "[", signature(x = "antsImage", i = "NULL", j = "NULL"),
 definition = function(x, i, j, k = NA, l = NA, ..., drop ) {
  return(antsGetPixels(x, i, j, k, l))
})

#' @describeIn as.antsImage
setMethod("[", signature(x = "antsImage", i = "numeric", j = "numeric", "ANY"),
 definition = function(x, i, j, k = NA, l = NA, ..., drop) {
  return(antsGetPixels(x, i, j, k, l))
})

#' @describeIn as.antsImage
setMethod("[", signature(x = "antsImage", i = "numeric", j = "numeric"),
 definition = function(x, i, j, k = NA, l = NA, ..., drop) {
  return(antsGetPixels(x, i, j, k, l))
})

#' @describeIn as.antsImage
setMethod(f = "[", signature(x = "antsImage", i = "numeric", j = "NULL", "ANY"),
 definition = function(x, i, j, k = NA, l = NA, ..., drop) {
  return(antsGetPixels(x, i, j, k, l))
})

#' @describeIn as.antsImage
setMethod(f = "[", signature(x = "antsImage", i = "numeric", j = "NULL"),
 definition = function(x, i, j, k = NA, l = NA, ..., drop) {
  return(antsGetPixels(x, i, j, k, l))
})

#' @describeIn as.antsImage
setMethod(f = "[", signature(x = "antsImage", i = "NULL", j = "numeric", "ANY"),
 definition = function(x, i, j, k = NA, l = NA, ..., drop) {
  return(antsGetPixels(x, i, j, k, l))
})

#' @describeIn as.antsImage
setMethod(f = "[", signature(x = "antsImage", i = "NULL", j = "numeric"),
 definition = function(x, i, j, k = NA, l = NA, ..., drop) {
  return(antsGetPixels(x, i, j, k, l))
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
    print("'mask' provided is not of type 'logical'")
    return()
  }
  region <- new("antsRegion", index = integer(), size = integer())
  return(.Call("antsImage_SetRegion", x, i, region, value, PACKAGE = "ANTsR"))
})


#' @describeIn as.antsImage
setMethod(f = "[<-", signature(x = "antsImage", i = "matrix"),
  definition = function(x, i, j, ..., value) {
  if (typeof(i) != "logical") {
    print("'mask' provided is not of type 'logical'")
    return()
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
    print("'mask' provided is not of type 'logical'")
    return()
  }
  if (class(i$region) != "antsRegion") {
    print("'region' provided is not of class 'antsRegion'")
    return()
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
    print("'mask' provided is not of type 'logical'")
    return()
  }
  return(.Call("antsImage_SetRegion", x, i, j, value, PACKAGE = "ANTsR"))
})

#' @describeIn as.antsImage
setMethod(f = "[<-", signature(x = "antsImage", i = "matrix", j = "antsRegion"),
  definition = function(x, i, j, ..., value) {
    if (typeof(i) != "logical") {
      print("'mask' provided is not of type 'logical'")
      return()
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
      print("indices must be of class 'integer' or 'numeric'")
      return()
    }
  }

  if (length(j) != 1 || !is.na(j)) {
    if (is.null(j)) {
      lst <- c(lst, list(integer(0)))
    } else if (class(j) == "integer" || class(j) == "numeric") {
      lst <- c(lst, list(j))
    } else {
      print("indices must be of class 'integer' or 'numeric'")
      return()
    }
  }

  if (length(k) != 1 || !is.na(k)) {
    if (is.null(k)) {
      lst <- c(lst, list(integer(0)))
    } else if (class(k) == "integer" || class(k) == "numeric") {
      lst <- c(lst, list(k))
    } else {
      print("indices must be of class 'integer' or 'numeric'")
      return()
    }
  }

  if (length(l) != 1 || !is.na(l)) {
    if (is.null(l)) {
      lst <- c(lst, list(integer(0)))
    } else if (class(l) == "integer" || class(l) == "numeric") {
      lst <- c(lst, list(l))
    } else {
      print("indices must be of class 'integer' or 'numeric'")
      return()
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
      print("length of value must be 1")
      return()
    }
    if (class(e2$region) != "antsRegion") {
      print("region argument not of class 'antsRegion'")
      return()
    }
    return(.Call("antsImage_RelationalOperators", e1, e2$value, e2$region, operator,
      PACKAGE = "ANTsR"))
  } else if (class(e2) == "numeric" && length(e2) == 1) {
    region <- new("antsRegion", index = integer(), size = integer())
    return(.Call("antsImage_RelationalOperators", e1, e2, region, operator, PACKAGE = "ANTsR"))
  } else {
    print("rhs must be a scalar or a list( <scalar> , <antsRegion> )")
    return()
  }
})


#' @describeIn antsImage
setMethod(f = "!=", signature(e1 = "antsImage"), definition = function(e1, e2) {
  operator <- "!="
  if (class(e2) == "list") {
    if (length(e2$value) != 1) {
      print("length of value must be 1")
      return()
    }
    if (class(e2$region) != "antsRegion") {
      print("region argument not of class 'antsRegion'")
      return()
    }
    return(.Call("antsImage_RelationalOperators", e1, e2$value, e2$region, operator,
      PACKAGE = "ANTsR"))
  } else if (class(e2) == "numeric" && length(e2) == 1) {
    region <- new("antsRegion", index = integer(), size = integer())
    return(.Call("antsImage_RelationalOperators", e1, e2, region, operator, PACKAGE = "ANTsR"))
  } else {
    print("rhs must be a scalar or a list( <scalar> , <antsRegion> )")
    return()
  }
})

#' @describeIn antsImage
setMethod(f = "<=", signature(e1 = "antsImage"), definition = function(e1, e2) {
  operator <- "<="
  if (class(e2) == "list") {
    if (length(e2$value) != 1) {
      print("length of value must be 1")
      return()
    }
    if (class(e2$region) != "antsRegion") {
      print("region argument not of class 'antsRegion'")
      return()
    }
    return(.Call("antsImage_RelationalOperators", e1, e2$value, e2$region, operator,
      PACKAGE = "ANTsR"))
  } else if (class(e2) == "numeric" && length(e2) == 1) {
    region <- new("antsRegion", index = integer(), size = integer())
    return(.Call("antsImage_RelationalOperators", e1, e2, region, operator, PACKAGE = "ANTsR"))
  } else {
    print("rhs must be a scalar or a list( <scalar> , <antsRegion> )")
    return()
  }
})

#' @describeIn antsImage
setMethod(f = ">=", signature(e1 = "antsImage"), definition = function(e1, e2) {
  operator <- ">="
  if (class(e2) == "list") {
    if (length(e2$value) != 1) {
      print("length of value must be 1")
      return()
    }
    if (class(e2$region) != "antsRegion") {
      print("region argument not of class 'antsRegion'")
      return()
    }
    return(.Call("antsImage_RelationalOperators", e1, e2$value, e2$region, operator,
      PACKAGE = "ANTsR"))
  } else if (class(e2) == "numeric" && length(e2) == 1) {
    region <- new("antsRegion", index = integer(), size = integer())
    return(.Call("antsImage_RelationalOperators", e1, e2, region, operator, PACKAGE = "ANTsR"))
  } else {
    print("rhs must be a scalar or a list( <scalar> , <antsRegion> )")
    return()
  }
})

#' @describeIn antsImage
setMethod(f = "<", signature(e1 = "antsImage"), definition = function(e1, e2) {
  operator <- "<"
  if (class(e2) == "list") {
    if (length(e2$value) != 1) {
      print("length of value must be 1")
      return()
    }
    if (class(e2$region) != "antsRegion") {
      print("region argument not of class 'antsRegion'")
      return()
    }
    return(.Call("antsImage_RelationalOperators", e1, e2$value, e2$region, operator,
      PACKAGE = "ANTsR"))
  } else if (class(e2) == "numeric" && length(e2) == 1) {
    region <- new("antsRegion", index = integer(), size = integer())
    return(.Call("antsImage_RelationalOperators", e1, e2, region, operator, PACKAGE = "ANTsR"))
  } else {
    print("rhs must be a scalar or a list( <scalar> , <antsRegion> )")
    return()
  }
})

#' @describeIn antsImage
setMethod(f = ">", signature(e1 = "antsImage"), definition = function(e1, e2) {
  operator <- ">"
  if (class(e2) == "list") {
    if (length(e2$value) != 1) {
      print("length of value must be 1")
      return()
    }
    if (class(e2$region) != "antsRegion") {
      print("region argument not of class 'antsRegion'")
      return()
    }
    return(.Call("antsImage_RelationalOperators", e1, e2$value, e2$region, operator,
      PACKAGE = "ANTsR"))
  } else if (class(e2) == "numeric" && length(e2) == 1) {
    region <- new("antsRegion", index = integer(), size = integer())
    return(.Call("antsImage_RelationalOperators", e1, e2, region, operator, PACKAGE = "ANTsR"))
  } else {
    print("rhs must be a scalar or a list( <scalar> , <antsRegion> )")
    return()
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
