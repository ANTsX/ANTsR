# see https://github.com/klutometis/roxygen/issues/272

#' @title Extract or Replace Parts of antsImage Object
#' @description Operators acting on antsImage objects
#' to extract or replace parts.
#' @rdname brackets
#'
#' @param x antsImage
#' @param i logical or i-th dimension
#' @param j not used or j-th dimension
#' @param drop method for missing data
#' @param k not used or k-th dimension
#' @param l not used or l-th dimension
#' @param ... not used
#' @param value value to assign to the subset in assignment operator
#'
#' @aliases [,antsImage,NULL,ANY,ANY-method
#' @examples
#' fi <- antsImageRead(getANTsRData( "r16" ))
#' fi[, 1]
#' fi[, 2:5]
setMethod(
  "[",
  c("antsImage", "NULL", "ANY", "ANY"),
  definition = function(x, i, j, ..., drop) {
    mask <- logical(0)
    region <-
      new("antsRegion", index = integer(), size = integer())
    return(ANTsRCore::antsImage_asVector(x, mask, region))
  }
)


#' @rdname brackets
#' @aliases [,antsImage,antsImage,ANY,ANY-method
#' @examples
#' fi <- antsImageRead(getANTsRData( "r16" ))
#' fi[ fi > 45]
setMethod(
  f = "[",
  signature(x = "antsImage", i = "antsImage", j = "ANY", "ANY"),
  definition = function(x, i, j, ..., drop) {
    i = coerce_mask(i)
    if (typeof(i) != "logical") {
      stop("'mask' provided is not of type 'logical'")
    }
    region <- new("antsRegion", index = integer(), size = integer())
    return(ANTsRCore::antsImage_asVector(x, i, region))
  }
)

#' @rdname brackets
#' @aliases [,ANY,antsImage,ANY,ANY-method
setMethod(
  f = "[",
  signature(x = "ANY", i = "antsImage", j = "ANY", "ANY"),
  definition = function(x, i, j, ..., drop) {
    i = coerce_mask(i)
    if (typeof(i) != "logical") {
      stop("'mask' provided is not of type 'logical'")
    }
    args = list(x = x, i = i, ... = ..., drop = drop)
    if (!missing(j)) {
      args$j = j
    }
    do.call("[", args)
  }
)


#' @rdname brackets
#' @aliases [,antsImage,logical,ANY,ANY-method
#' @examples
#' fi <- antsImageRead(getANTsRData( "r16" ))
#' arr = c(as.array(fi) > 45)
#' fi[arr]
#' fi[ 1:10, 2:14]
#' fi[ 1:10*1.0, 2:14*1.0]
setMethod(
  f = "[",
  signature(x = "antsImage", i = "logical", j = "ANY", "ANY"),
  definition = function(x, i, j, ..., drop) {
    region <- new("antsRegion", index = integer(), size = integer())
    return(ANTsRCore::antsImage_asVector(x, i, region))
  }
)

#' @rdname brackets
#' @aliases [,antsImage,logical,ANY-method
setMethod(
  f = "[",
  signature(x = "antsImage", i = "logical", j = "ANY"),
  definition = function(x, i, j, ..., drop) {
    region <- new("antsRegion", index = integer(), size = integer())
    return(ANTsRCore::antsImage_asVector(x, i, region))
  }
)


#' @rdname brackets
#' @aliases [,antsImage,ANY,ANY,ANY-method
setMethod(
  f = "[",
  signature(x = "antsImage", i = "ANY", j = "ANY", "ANY"),
  definition = function(x, i, j, ..., drop) {
    if (typeof(i) != "logical") {
      stop("'mask' provided is not of type 'logical'")
    }
    region <-
      new("antsRegion", index = integer(), size = integer())
    return(ANTsRCore::antsImage_asVector(x, i, region))
  }
)

#' @rdname brackets
#' @aliases [,antsImage,ANY,ANY-method
setMethod(
  f = "[",
  signature(x = "antsImage", i = "ANY", j = "ANY"),
  definition = function(x, i, j, ..., drop) {
    if (typeof(i) != "logical") {
      stop("'mask' provided is not of type 'logical'")
    }
    region <-
      new("antsRegion", index = integer(), size = integer())
    return(ANTsRCore::antsImage_asVector(x, i, region))
  }
)



#' @rdname brackets
#' @aliases [,antsImage,NULL,NULL,ANY-method
setMethod(
  f = "[",
  signature(x = "antsImage", i = "NULL", j = "NULL", "ANY"),
  definition = function(x,
                        i,
                        j,
                        k = NA,
                        l = NA,
                        ...,
                        drop) {
    return(getPixels(x, i, j, k, l))
  }
)

#' @rdname brackets
#' @aliases [,antsImage,NULL,NULL-method
setMethod(
  f = "[",
  signature(x = "antsImage", i = "NULL", j = "NULL"),
  definition = function(x,
                        i,
                        j,
                        k = NA,
                        l = NA,
                        ...,
                        drop) {
    return(getPixels(x, i, j, k, l))
  }
)

#' @rdname brackets
#' @aliases [,antsImage,numeric,numeric,ANY-method
setMethod(
  "[",
  signature(x = "antsImage", i = "numeric", j = "numeric", "ANY"),
  definition = function(x,
                        i,
                        j,
                        k = NA,
                        l = NA,
                        ...,
                        drop) {
    dx = dim(x)
    ndim = length(dx)
    if (ndim > 2) {
      if (missing(k) || is.na(k)) {
        k = seq(dx[3])
      }
    }
    if (ndim > 3) {
      if (missing(l) || is.na(l)) {
        l = seq(dx[4])
      }
    }    
    return(getPixels(x, i, j, k, l))
  }
)

#' @rdname brackets
#' @aliases [,antsImage,numeric,numeric-method
setMethod(
  "[",
  signature(x = "antsImage", i = "numeric", j = "numeric"),
  definition = function(x,
                        i,
                        j,
                        k = NA,
                        l = NA,
                        ...,
                        drop) {
    dx = dim(x)
    ndim = length(dx)
    if (ndim > 2) {
      if (missing(k) || is.na(k)) {
        k = seq(dx[3])
      }
    }
    if (ndim > 3) {
      if (missing(l) || is.na(l)) {
        l = seq(dx[4])
      }
    }       
    return(getPixels(x, i, j, k, l))
  }
)

#' @rdname brackets
#' @aliases [,antsImage,numeric,NULL,ANY-method
setMethod(
  f = "[",
  signature(x = "antsImage", i = "numeric", j = "NULL", "ANY"),
  definition = function(x,
                        i,
                        j,
                        k = NA,
                        l = NA,
                        ...,
                        drop) {
    dx = dim(x)
    ndim = length(dx)
    if (ndim > 2) {
      if (missing(k) || is.na(k)) {
        k = seq(dx[3])
      }
    }
    if (ndim > 3) {
      if (missing(l) || is.na(l)) {
        l = seq(dx[4])
      }
    }         
    return(getPixels(x, i, j, k, l))
  }
)

#' @rdname brackets
#' @aliases [,antsImage,numeric,NULL-method
setMethod(
  f = "[",
  signature(x = "antsImage", i = "numeric", j = "NULL"),
  definition = function(x,
                        i,
                        j,
                        k = NA,
                        l = NA,
                        ...,
                        drop) {
    dx = dim(x)
    ndim = length(dx)
    if (ndim > 2) {
      if (missing(k) || is.na(k)) {
        k = seq(dx[3])
      }
    }
    if (ndim > 3) {
      if (missing(l) || is.na(l)) {
        l = seq(dx[4])
      }
    }         
    return(getPixels(x, i, j, k, l))
  }
)

#' @rdname brackets
#' @aliases [,antsImage,NULL,numeric,ANY-method
setMethod(
  f = "[",
  signature(x = "antsImage", i = "NULL", j = "numeric", "ANY"),
  definition = function(x,
                        i,
                        j,
                        k = NA,
                        l = NA,
                        ...,
                        drop) {
    dx = dim(x)
    ndim = length(dx)
    if (ndim > 2) {
      if (missing(k) || is.na(k)) {
        k = seq(dx[3])
      }
    }
    if (ndim > 3) {
      if (missing(l) || is.na(l)) {
        l = seq(dx[4])
      }
    }        
    return(getPixels(x, i, j, k, l))
  }
)

#' @rdname brackets
#' @aliases [,antsImage,NULL,numeric-method
setMethod(
  f = "[",
  signature(x = "antsImage", i = "NULL", j = "numeric"),
  definition = function(x,
                        i,
                        j,
                        k = NA,
                        l = NA,
                        ...,
                        drop) {
    dx = dim(x)
    ndim = length(dx)
    if (ndim > 2) {
      if (missing(k) || is.na(k)) {
        k = seq(dx[3])
      }
    }
    if (ndim > 3) {
      if (missing(l) || is.na(l)) {
        l = seq(dx[4])
      }
    }         
    return(getPixels(x, i, j, k, l))
  }
)

#' @rdname brackets
#' @aliases [,antsImage,missing,numeric-method
setMethod(
  f = "[",
  signature(x = "antsImage", i = "missing", j = "numeric"),
  definition = function(x,
                        i,
                        j,
                        k = NA,
                        l = NA,
                        ...,
                        drop) {
    i <- seq(dim(x)[1])
    dx = dim(x)
    ndim = length(dx)
    if (ndim > 2) {
      if (missing(k) || is.na(k)) {
        k = seq(dx[3])
      }
    }
    if (ndim > 3) {
      if (missing(l) || is.na(l)) {
        l = seq(dx[4])
      }
    }         
    return(getPixels(x, i, j, k, l))
  }
)

#' @rdname brackets
#' @aliases [,antsImage,numeric,missing-method
setMethod(
  f = "[",
  signature(x = "antsImage", i = "numeric", j = "missing"),
  definition = function(x,
                        i,
                        j,
                        k = NA,
                        l = NA,
                        ...,
                        drop) {
    j <- seq(dim(x)[2])
    dx = dim(x)
    ndim = length(dx)
    if (ndim > 2) {
      if (missing(k) || is.na(k)) {
        k = seq(dx[3])
      }
    }
    if (ndim > 3) {
      if (missing(l) || is.na(l)) {
        l = seq(dx[4])
      }
    }        
    return(getPixels(x, i, j, k, l))
  }
)

#' @rdname brackets
#' @aliases [,antsImage,missing,missing-method
#' @examples 
#' fi <- antsImageRead(getANTsRData( "ch2" ))
#' fi[, , 2] 
setMethod(
  f = "[",
  signature(x = "antsImage", i = "missing", j = "missing"),
  definition = function(x,
                        i,
                        j,
                        k = NA,
                        l = NA,
                        ...,
                        drop) {
    i <- seq(dim(x)[1])
    j <- seq(dim(x)[2])
    
    # should fix the subsetting
    dx = dim(x)
    ndim = length(dx)
    if (ndim > 2) {
      if (missing(k) || is.na(k)) {
        k = seq(dx[3])
      }
    }
    if (ndim > 3) {
      if (missing(l) || is.na(l)) {
        l = seq(dx[4])
      }
    }  
    return(getPixels(x, i, j, k, l))
  }
)


#' @rdname subset
#' @title Subsetting antsImage Objects
#' @description Returns subsets of antsImage objects as vectors
#'
#' @param x antsImage object
#' @param subset logical expression of subset
#' @param ... further arguments to be passed to or from other methods.
#'
#' @export
#' @method subset antsImage
subset.antsImage = function(x, subset, ...) {
  if (is.antsImage(subset)) {
    subset = c(coerce_mask(subset))
  }
  x = as.array(x)
  base::subset(x = x, subset = subset, ...)
}
