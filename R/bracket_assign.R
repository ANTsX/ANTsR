

# > getGeneric("[<-")
# standardGeneric for "[<-" defined from package "base"
# function (x, i, j, ..., value)

#' @describeIn as.antsImage
#' @aliases [<-,antsImage,NULL-method
setMethod(
  f = "[<-",
  signature(x = "antsImage", i = "NULL"),
  definition = function(x, i, j, ..., value) {
    mask <- logical(0)
    region <-
      new("antsRegion", index = integer(), size = integer())
    return(.Call("antsImage_SetRegion", x, mask, region, value, PACKAGE = "ANTsR"))
  }
)

#' @describeIn as.antsImage
#' @aliases [<-,antsImage,logical-method
setMethod(
  f = "[<-",
  signature(x = "antsImage", i = "logical"),
  definition = function(x, i, j, ..., value) {
    region <- new("antsRegion", index = integer(), size = integer())
    return(.Call("antsImage_SetRegion", x, i, region, value, PACKAGE = "ANTsR"))
  }
)

#' @describeIn as.antsImage
#' @aliases [<-,antsImage,array-method
setMethod(
  f = "[<-",
  signature(x = "antsImage", i = "array"),
  definition = function(x, i, j, ..., value) {
    if (typeof(i) != "logical") {
      stop("'mask' provided is not of type 'logical'")
    }
    region <-
      new("antsRegion", index = integer(), size = integer())
    return(.Call("antsImage_SetRegion", x, i, region, value, PACKAGE = "ANTsR"))
  }
)


#' @describeIn as.antsImage
#' @aliases [<-,antsImage,matrix-method
setMethod(
  f = "[<-",
  signature(x = "antsImage", i = "matrix"),
  definition = function(x, i, j, ..., value) {
    if (typeof(i) != "logical") {
      stop("'mask' provided is not of type 'logical'")
    }
    region <-
      new("antsRegion", index = integer(), size = integer())
    return(.Call("antsImage_SetRegion", x, i, region, value, PACKAGE = "ANTsR"))
  }
)

#' @describeIn as.antsImage
#' @aliases [<-,antsImage,list-method
setMethod(
  f = "[<-",
  signature(x = "antsImage", i = "list"),
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
  }
)

#' @describeIn as.antsImage
#' @aliases [<-,antsImage,NULL,antsRegion-method
setMethod(
  f = "[<-",
  signature(x = "antsImage", i = "NULL", j = "antsRegion"),
  definition = function(x, i, j, ..., value) {
    mask <- logical(0)
    return(.Call("antsImage_SetRegion", x, mask, j, value, PACKAGE = "ANTsR"))
  }
)

#' @describeIn as.antsImage
#' @aliases [<-,antsImage,logical,antsRegion-method
setMethod(
  f = "[<-",
  signature(x = "antsImage", i = "logical", j = "antsRegion"),
  definition = function(x, i, j, ..., value) {
    return(.Call("antsImage_SetRegion", x, i, j, value, PACKAGE = "ANTsR"))
  }
)


#' @describeIn as.antsImage
#' @aliases [<-,antsImage,array,antsRegion-method
setMethod(
  f = "[<-",
  signature(x = "antsImage", i = "array", j = "antsRegion"),
  definition = function(x, i, j, ..., value) {
    if (typeof(i) != "logical") {
      stop("'mask' provided is not of type 'logical'")
    }
    return(.Call("antsImage_SetRegion", x, i, j, value, PACKAGE = "ANTsR"))
  }
)

#' @describeIn as.antsImage
#' @aliases [<-,antsImage,matrix,antsRegion-method
setMethod(
  f = "[<-",
  signature(x = "antsImage", i = "matrix", j = "antsRegion"),
  definition = function(x, i, j, ..., value) {
    if (typeof(i) != "logical") {
      stop("'mask' provided is not of type 'logical'")
    }
    return(.Call("antsImage_SetRegion", x, i, j, value, PACKAGE = "ANTsR"))
  }
)


#' @describeIn as.antsImage
#' @aliases [<-,antsImage,NULL,NULL,numeric-method
setMethod(
  f = "[<-",
  signature(
    x = "antsImage",
    i = "NULL",
    j = "NULL",
    value = "numeric"
  ),
  definition = function(x, i, j, ..., value) {
    temp <- antsSetPixels(x, i, j, ..., value = value)
    return(temp)
  }
)

#' @describeIn as.antsImage
#' @aliases [<-,antsImage,numeric,numeric,numeric-method
setMethod(
  f = "[<-",
  signature(
    x = "antsImage",
    i = "numeric",
    j = "numeric",
    value = "numeric"
  ),
  definition = function(x, i, j, ..., value) {
    temp <- antsSetPixels(x, i, j, ..., value = value)
    return(temp)
  }
)

#' @describeIn as.antsImage
#' @aliases [<-,antsImage,numeric,NULL,numeric-method
setMethod(
  f = "[<-",
  signature(
    x = "antsImage",
    i = "numeric",
    j = "NULL",
    value = "numeric"
  ),
  definition = function(x, i, j, ..., value) {
    temp <- antsSetPixels(x, i, j, ..., value = value)
    return(temp)
  }
)


#' @describeIn as.antsImage
#' @aliases [<-,antsImage,NULL,numeric,numeric-method
setMethod(
  f = "[<-",
  signature(
    x = "antsImage",
    i = "NULL",
    j = "numeric",
    value = "numeric"
  ),
  definition = function(x, i, j, ..., value) {
    temp <- antsSetPixels(x, i, j, ..., value = value)
    return(temp)
  }
)
