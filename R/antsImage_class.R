# this file defines the S4 classes related to 'antsImage' and their associated
# methods

#' An S4 class to hold a region of an antsImage
#'
#' @slot index
#' @slot size
setClass(Class = "antsRegion", representation(index = "numeric", size = "numeric"))


#' An S4 class for an antsImage
#'
#' C++ type used to represent an ITK image pointed to by 'pointer'. the
#' actual image is of C++ type 'itk::image< pixeltype , dimension >::Pointer'
#'
#' @slot pixeltype usually float, can be other types unsigned char, int, double
#' etc noting that short is not supported
#' @slot dimension usually 2 or 3 but can be 4
setClass(Class = "antsImage", representation(pixeltype = "character", dimension = "integer",
  pointer = "externalptr"))

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
setMethod(f = "as.array", signature(x = "antsImage"),
 definition = function(x, mask = logical(),
  region = new("antsRegion", index = integer(), size = integer())) {
  if (typeof(mask) != "logical") {
    print("'mask' provided is not of type 'logical'")
    return()
  }
  return(.Call("antsImage_asVector", x, mask, region, PACKAGE = "ANTsR"))
})

#' @describeIn antsImage
setMethod(f = "[", signature(x = "antsImage", i = "NULL"),
  definition = function(x, i) {
  mask <- logical(0)
  region <- new("antsRegion", index = integer(), size = integer())
  return(.Call("antsImage_asVector", x, mask, region, PACKAGE = "ANTsR"))
})

#' @describeIn antsImage
setMethod(f = "[", signature(x = "antsImage", i = "logical"),
  definition = function(x, i) {
  region <- new("antsRegion", index = integer(), size = integer())
  return(.Call("antsImage_asVector", x, i, region, PACKAGE = "ANTsR"))
})

#' @describeIn antsImage
setMethod(f = "[", signature(x = "antsImage", i = "array"),
  definition = function(x, i) {
  if (typeof(i) != "logical") {
    print("'mask' provided is not of type 'logical'")
    return()
  }
  region <- new("antsRegion", index = integer(), size = integer())
  return(.Call("antsImage_asVector", x, i, region, PACKAGE = "ANTsR"))
})

#' @describeIn antsImage
setMethod(f = "[", signature(x = "antsImage", i = "matrix"),
  definition = function(x, i) {
  if (typeof(i) != "logical") {
    print("'mask' provided is not of type 'logical'")
    return()
  }
  region <- new("antsRegion", index = integer(), size = integer())
  return(.Call("antsImage_asVector", x, i, region, PACKAGE = "ANTsR"))
})

#' @describeIn antsImage
setMethod(f = "[", signature(x = "antsImage", i = "list"),
  definition = function(x, i) {
  if (class(i$mask) == "NULL") {
    i$mask <- logical(0)
  } else if (typeof(i$mask) != "logical") {
    print("'mask' provided is not of type 'logical' or 'NULL'")
    return()
  }
  if (class(i$region) != "antsRegion") {
    print("'region' provided is not of class 'antsRegion'")
    return()
  }
  return(.Call("antsImage_asVector", x, i$mask, i$region, PACKAGE = "ANTsR"))
})

#' @describeIn antsImage
setMethod(f = "[", signature(x = "antsImage", i = "NULL", j = "antsRegion"),
  definition = function(x,  i, j) {
  mask <- logical(0)
  return(.Call("antsImage_asVector", x, mask, j, PACKAGE = "ANTsR"))
})

#' @describeIn antsImage
setMethod(f = "[", signature(x = "antsImage", i = "logical", j = "antsRegion"), definition = function(x,
  i, j) {
  return(.Call("antsImage_asVector", x, i, j, PACKAGE = "ANTsR"))
})

#' @describeIn antsImage
setMethod(f = "[", signature(x = "antsImage", i = "array", j = "antsRegion"), definition = function(x,
  i, j) {
  if (typeof(i) != "logical") {
    print("'mask' provided is not of type 'logical'")
    return()
  }
  return(.Call("antsImage_asVector", x, i, j, PACKAGE = "ANTsR"))
})

#' @describeIn antsImage
setMethod(f = "[", signature(x = "antsImage", i = "matrix", j = "antsRegion"), definition = function(x,
  i, j) {
  if (typeof(i) != "logical") {
    print("'mask' provided is not of type 'logical'")
    return()
  }
  return(.Call("antsImage_asVector", x, i, j, PACKAGE = "ANTsR"))
})


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

antsGetSpacing <- function(x) {
  if (class(x)[1] != "antsImage") {
    print("Input must be of class 'antsImage'")
    return()
  }

  return(.Call("antsImage_GetSpacing", x, PACKAGE = "ANTsR"))
}

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

antsGetOrigin <- function(x) {
  if (class(x)[1] != "antsImage") {
    print("Input must be of class 'antsImage'")
    return()
  }
  return(.Call("antsImage_GetOrigin", x, PACKAGE = "ANTsR"))
}

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

antsGetDirection <- function(x) {
  if (class(x)[1] != "antsImage") {
    print("Input must be of class 'antsImage'")
    return()
  }
  return(.Call("antsImage_GetDirection", x, PACKAGE = "ANTsR"))
}

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
#' mnit<-getANTsRData('mni')
#' mnit<-antsImageRead(mnit,3)
#' center <- dim(mnit)/2
#' radius <- rep(5,3)
#' mat<-antsGetNeighborhood(mnit,center,radius)
#'
#'
#' @export antsGetNeighborhood
antsGetNeighborhood <- function(x, center, radius, physical.coordinates = FALSE) {

  if (class(x)[1] != "antsImage") {
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

  return(.Call("antsImage_GetNeighborhood", x, center, radius, physical.coordinates))
}







#' Get Neighborhood
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
#' mnit<-getANTsRData('mni')
#' mnit<-antsImageRead(mnit,3)
#' mask<-getMask(mnit,lowThresh=mean(mnit),cleanup=TRUE)
#' center <- dim(mnit)/2
#' radius <- rep(2,3)
#' mat<-antsGetNeighborhoodMatrix(mnit,mask,radius)
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

setMethod(f = "[", signature(x = "antsImage", i = "NULL", j = "NULL"), definition = function(x,
  i, j, k = NA, l = NA) {
  return(antsGetPixels(x, i, j, k, l))
})

setMethod(f = "[", signature(x = "antsImage", i = "numeric", j = "numeric"), definition = function(x,
  i, j, k = NA, l = NA) {
  return(antsGetPixels(x, i, j, k, l))
})

setMethod(f = "[", signature(x = "antsImage", i = "numeric", j = "NULL"), definition = function(x,
  i, j, k = NA, l = NA) {
  return(antsGetPixels(x, i, j, k, l))
})

setMethod(f = "[", signature(x = "antsImage", i = "NULL", j = "numeric"), definition = function(x,
  i, j, k = NA, l = NA) {
  return(antsGetPixels(x, i, j, k, l))
})

setMethod(f = "[<-", signature(x = "antsImage", i = "NULL"), definition = function(x,
  i, value) {
  mask <- logical(0)
  region <- new("antsRegion", index = integer(), size = integer())
  return(.Call("antsImage_SetRegion", x, mask, region, value, PACKAGE = "ANTsR"))
})

setMethod(f = "[<-", signature(x = "antsImage", i = "logical"), definition = function(x,
  i, value) {
  region <- new("antsRegion", index = integer(), size = integer())
  return(.Call("antsImage_SetRegion", x, i, region, value, PACKAGE = "ANTsR"))
})

setMethod(f = "[<-", signature(x = "antsImage", i = "array"), definition = function(x,
  i, value) {
  if (typeof(i) != "logical") {
    print("'mask' provided is not of type 'logical'")
    return()
  }
  region <- new("antsRegion", index = integer(), size = integer())
  return(.Call("antsImage_SetRegion", x, i, region, value, PACKAGE = "ANTsR"))
})

setMethod(f = "[<-", signature(x = "antsImage", i = "matrix"), definition = function(x,
  i, value) {
  if (typeof(i) != "logical") {
    print("'mask' provided is not of type 'logical'")
    return()
  }
  region <- new("antsRegion", index = integer(), size = integer())
  return(.Call("antsImage_SetRegion", x, i, region, value, PACKAGE = "ANTsR"))
})

setMethod(f = "[<-", signature(x = "antsImage", i = "list"), definition = function(x,
  i, value) {
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

setMethod(f = "[<-", signature(x = "antsImage", i = "NULL", j = "antsRegion"), definition = function(x,
  i, j, value) {
  mask <- logical(0)
  return(.Call("antsImage_SetRegion", x, mask, j, value, PACKAGE = "ANTsR"))
})

setMethod(f = "[<-", signature(x = "antsImage", i = "logical", j = "antsRegion"),
  definition = function(x, i, j, value) {
    return(.Call("antsImage_SetRegion", x, i, j, value, PACKAGE = "ANTsR"))
  })

setMethod(f = "[<-", signature(x = "antsImage", i = "array", j = "antsRegion"), definition = function(x,
  i, j, value) {
  if (typeof(i) != "logical") {
    print("'mask' provided is not of type 'logical'")
    return()
  }
  return(.Call("antsImage_SetRegion", x, i, j, value, PACKAGE = "ANTsR"))
})

setMethod(f = "[<-", signature(x = "antsImage", i = "matrix", j = "antsRegion"),
  definition = function(x, i, j, value) {
    if (typeof(i) != "logical") {
      print("'mask' provided is not of type 'logical'")
      return()
    }
    return(.Call("antsImage_SetRegion", x, i, j, value, PACKAGE = "ANTsR"))
  })

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
  return(.Call("antsImage_SetPixels", x, lst, value, PACKAGE = "ANTsR"))
}

setMethod(f = "[<-", signature(x = "antsImage", i = "NULL", j = "NULL", value = "numeric"),
  definition = function(x, i, j, ..., value) {
    return(antsSetPixels(x, i, j, ..., value = value))
  })

setMethod(f = "[<-", signature(x = "antsImage", i = "numeric", j = "numeric", value = "numeric"),
  definition = function(x, i, j, ..., value) {
    return(antsSetPixels(x, i, j, ..., value = value))
  })

setMethod(f = "[<-", signature(x = "antsImage", i = "numeric", j = "NULL", value = "numeric"),
  definition = function(x, i, j, ..., value) {
    return(antsSetPixels(x, i, j, ..., value = value))
  })

setMethod(f = "[<-", signature(x = "antsImage", i = "NULL", j = "numeric", value = "numeric"),
  definition = function(x, i, j, ..., value) {
    return(antsSetPixels(x, i, j, ..., value = value))
  })

#' as.antsImage
#'
#' convert types to antsImage
#'
#' @param object An object
#' @param data Numeric vector or data.frame
#' @param Fun Function. Default function is \code{sum}
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

###################################################################################################

#' An S4 class to hold a list of antsImages
#'
#' @slot pixeltype
#' @slot dimension
setClass(Class = "antsImageList", representation(pixeltype = "character", dimension = "integer",
  pointer = "externalptr"))

setMethod(f = "initialize", signature(.Object = "antsImageList"), definition = function(.Object,
  pixeltype = "float", dimension = 3) {
  .Call("antsImageList", pixeltype, dimension, PACKAGE = "ANTsR")
})

setMethod(f = "as.list", signature(x = "antsImageList"), definition = function(x) {
  return(.Call("antsImageList_asList", x, PACKAGE = "ANTsR"))
})
