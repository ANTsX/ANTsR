# see https://github.com/klutometis/roxygen/issues/272

#' ants image substitutions
#' @param x antsImage
#' @param i logical or i-th dimension
#' @param j not used or j-th dimension
#' @param drop method for missing data
#' @param k not used or k-th dimension
#' @param l not used or l-th dimension
#' @param value ok
#' @describeIn as.antsImage
#' @aliases [,antsImage,NULL,ANY,ANY-method
setMethod(
  "[",
  c("antsImage", "NULL", "ANY", "ANY"),
  definition = function(x, i, j, ..., drop) {
    mask <- logical(0)
    region <-
      new("antsRegion", index = integer(), size = integer())
    return(.Call("antsImage_asVector", x, mask, region, PACKAGE = "ANTsR"))
  }
)

#' @describeIn as.antsImage
#' @aliases [,antsImage,NULL,ANY-method
setMethod(
  "[",
  c("antsImage", "NULL", "ANY"),
  definition = function(x, i, j, ..., drop) {
    mask <- logical(0)
    region <-
      new("antsRegion", index = integer(), size = integer())
    return(.Call("antsImage_asVector", x, mask, region, PACKAGE = "ANTsR"))
  }
)


#' @describeIn as.antsImage
#' @aliases [,antsImage,logical,ANY,ANY-method
setMethod(
  f = "[",
  signature(x = "antsImage", i = "logical", j = "ANY", "ANY"),
  definition = function(x, i, j, ..., drop) {
    region <- new("antsRegion", index = integer(), size = integer())
    return(.Call("antsImage_asVector", x, i, region, PACKAGE = "ANTsR"))
  }
)

#' @describeIn as.antsImage
#' @aliases [,antsImage,logical,ANY-method
setMethod(
  f = "[",
  signature(x = "antsImage", i = "logical", j = "ANY"),
  definition = function(x, i, j, ..., drop) {
    region <- new("antsRegion", index = integer(), size = integer())
    return(.Call("antsImage_asVector", x, i, region, PACKAGE = "ANTsR"))
  }
)


#' @describeIn as.antsImage
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
    return(.Call("antsImage_asVector", x, i, region, PACKAGE = "ANTsR"))
  }
)

#' @describeIn as.antsImage
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
    return(.Call("antsImage_asVector", x, i, region, PACKAGE = "ANTsR"))
  }
)



#' @describeIn as.antsImage
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

#' @describeIn as.antsImage
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

#' @describeIn as.antsImage
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
    return(getPixels(x, i, j, k, l))
  }
)

#' @describeIn as.antsImage
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
    return(getPixels(x, i, j, k, l))
  }
)

#' @describeIn as.antsImage
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
    return(getPixels(x, i, j, k, l))
  }
)

#' @describeIn as.antsImage
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
    return(getPixels(x, i, j, k, l))
  }
)

#' @describeIn as.antsImage
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
    return(getPixels(x, i, j, k, l))
  }
)

#' @describeIn as.antsImage
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
    return(getPixels(x, i, j, k, l))
  }
)

#' @describeIn as.antsImage
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
    i <- 1:(dim(x)[1])
    return(getPixels(x, i, j, k, l))
  }
)

#' @describeIn as.antsImage
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
    j <- 1:(dim(x)[2])
    return(getPixels(x, i, j, k, l))
  }
)

#' @describeIn as.antsImage
#' @aliases [,antsImage,missing,missing-method
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
    i <- 1:(dim(x)[1])
    j <- 1:(dim(x)[2])
    return(getPixels(x, i, j, k, l))
  }
)
