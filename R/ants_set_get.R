
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
#' img<-makeImage(c(10,10),rnorm(100))
#' pixel<-getPixels(img,i=c(1,2),j=1)
#' @export
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
  
  # should fix the subsetting
  dx = dim(x)
  ndim = length(dx)
  if (ndim > 2) {
    if (missing(k)) {
      k = seq(dx[3])
    }
  }
  if (ndim > 3) {
    if (missing(l)) {
      l = seq(dx[4])
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
  return(.Call("antsImage_GetPixels", x, lst, PACKAGE = "ANTsRCore"))
}

#' @title antsImageGetSet
#' @description Get and set methods for image header information
#' @name antsImageGetSet
#' @rdname antsImageGetSet
#' @usage antsGetSpacing(x)
#' @param x antsImage to access, of dimensionality \code{d}.
#' @return For \code{get} methods, vector of length \code{d} (origin, spacing) or matrix of size \code{d * d} (direction).
#' For \code{set} methods, 0 to indicate success.
#' @export
#' @examples
#' img <- makeImage(c(5,5), rnorm(25))
#' antsGetSpacing(img)
#' antsSetSpacing(img, c(2.0, 2.0))
#' antsGetOrigin(img)
#' antsSetOrigin(img, c(0.5, 0.5))
#' testthat::expect_error(antsGetSpacing(as.array(img) ) )
#' testthat::expect_error(antsSetSpacing(as.array(img), c(2,2) ), "class" )
#' testthat::expect_error(antsSetSpacing(img, c("2",2) ), "numeric" )
#' testthat::expect_error(antsSetSpacing(img, c(3,3,3) ), "dimensions" )
#' 
#' testthat::expect_error(antsGetOrigin(as.array(img) ) )
#' testthat::expect_error(antsSetOrigin(as.array(img), c(0.5, 0.5) ) )
#' testthat::expect_error(antsSetOrigin(img, c("0.5", 0.5) ) )
#' testthat::expect_error(antsSetOrigin(img, c(0.5, 0.5, 0.5) ) )
antsGetSpacing <- function(x) {
  x = check_ants(x)
  if (!is.antsImage(x)) {
    stop("Input must be of class 'antsImage'")
  }
  
  return(.Call("antsImage_GetSpacing", x, PACKAGE = "ANTsRCore"))
}
#' @rdname antsImageGetSet
#' @param spacing numeric vector of length \code{d}.
#' @export
antsSetSpacing <- function(x, spacing) {
  x = check_ants(x)
  if (!is.antsImage(x)) {
    stop("Input must be of class 'antsImage'")
  }
  
  if ((class(spacing) != "numeric") && (class(spacing) != "array")) {
    stop("spacing must be of class 'numeric'")
  }
  
  if (length(spacing) != length(dim(x))) {
    stop("spacing must be of same dimensions as image")
  }
  
  return(.Call("antsImage_SetSpacing", x, spacing, PACKAGE = "ANTsRCore"))
}

#' @rdname antsImageGetSet
#' @usage antsGetOrigin(x)
#' @export
antsGetOrigin <- function(x) {
  x = check_ants(x)
  if (!is.antsImage(x)) {
    stop("Input must be of class 'antsImage'")
  }
  return(.Call("antsImage_GetOrigin", x, PACKAGE = "ANTsRCore"))
}
#' @rdname antsImageGetSet
#' @usage antsSetOrigin(x, origin)
#' @param origin numeric vector of length \code{d}.
#' @export
antsSetOrigin <- function(x, origin) {
  x = check_ants(x)
  if (!is.antsImage(x)) {
    stop("Input must be of class 'antsImage'")
  }
  if (!is.numeric(origin) && !is.array(origin)) {
    stop("spacing must be of class 'numeric' or 'array'")
  }
  
  if (length(origin) != length(dim(x))) {
    stop("spacing must be of same dimensions as image")
  }
  
  # res = .Call("antsImage_SetOrigin", x, origin, PACKAGE = "ANTsRCore")
  # if (res != 0) {
  #   warning("Setting origin did not have result 0, results may be wrong")
  # }
  return(.Call("antsImage_SetOrigin", x, origin, PACKAGE = "ANTsRCore"))
}

#' @rdname antsImageGetSet
#' @usage antsGetDirection(x)
#' @export
#' @examples
#' img <- makeImage(c(5,5), rnorm(25))
#' antsGetDirection(img)
#' testthat::expect_error(antsGetDirection(as.array(img) ) )

antsGetDirection <- function(x) {
  x = check_ants(x)
  if (!is.antsImage(x)) {
    stop("Input must be of class 'antsImage'")
  }
  return(.Call("antsImage_GetDirection", x, PACKAGE = "ANTsRCore"))
}

#' @rdname antsImageGetSet
#' @usage antsSetDirection(x, direction)
#' @param direction matrix of size \code{d * d}.
#' @export
#' @examples
#' outimg <- makeImage(c(5,5), rnorm(25))
#' antsGetDirection(outimg)
#' direct = antsGetDirection(outimg)
#' antsSetDirection(outimg, direct)
#' testthat::expect_error(antsSetDirection(as.array(outimg), direct) ) 
#' testthat::expect_error(antsSetDirection(outimg, as.numeric(direct)) ) 
#' testthat::expect_error(antsSetDirection(outimg, diag(length(dim(outimg))+1) ))
antsSetDirection <- function(x, direction) {
  x = check_ants(x)
  if (!is.antsImage(x)) {
    stop("Input must be of class 'antsImage'")
  }
  if (!is.matrix(direction) && !is.array(array)) {
    stop("direction must be of class 'matrix' or 'array'")
  }
  if ((dim(direction)[1] != length(dim(x))) || (dim(direction)[2] != length(dim(x)))) {
    stop("direction matrix must be of size imagedim * imagedim")
  }
  return(.Call("antsImage_SetDirection", x, direction, PACKAGE = "ANTsRCore"))
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
#' img<-makeImage(c(10,10),rnorm(100))
#' antsSetPixels(img,2,3,value=Inf)
#'
#'
#' @export
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
  returnList <- (.Call("antsImage_SetPixels", x, lst, value, PACKAGE = "ANTsRCore"))
  
  if ( returnList$flag > 0 ) {
    warning( returnList$error )
  }
  return( returnList$image )
}
