

# > getGeneric("[<-")
# standardGeneric for "[<-" defined from package "base"
# function (x, i, j, ..., value)

#' @rdname brackets
#' @aliases [<-,antsImage,NULL-method
setMethod(
  f = "[<-",
  signature(x = "antsImage", i = "NULL"),
  definition = function(x, i, j, ..., value) {
    mask <- logical(0)
    region <-
      new("antsRegion", index = integer(), size = integer())
    return(.Call("antsImage_SetRegion", x, mask, 
                 region, value, PACKAGE = "ANTsRCore"))
  }
)

#' @rdname brackets
#' @aliases [<-,antsImage,logical-method
#' @examples
#' fi <- antsImageRead(getANTsRData( "r16" ))
#' l = prod(dim(fi))
#' sub = sample(c(TRUE, FALSE), size = l, replace = TRUE)
#' fi[sub] = rnorm(sum(sub))
#' fi[,1] = array(10, dim = dim(fi[, 1]))
#' 
setMethod(
  f = "[<-",
  signature(x = "antsImage", i = "logical"),
  definition = function(x, i, j, ..., value) {
    region <- new("antsRegion", index = integer(), size = integer())
    return(.Call("antsImage_SetRegion", x, 
                 i, region, value, PACKAGE = "ANTsRCore"))
  }
)

#' @rdname brackets
#' @aliases [<-,antsImage,array-method
#' @examples
#' fi <- antsImageRead(getANTsRData( "ch2" ))
#' fi[2, 1, 2]
#' fi[2, 1, 2] = 4
#' fi[, 1, 2]
#' fi[, 1, 2] = array(10, dim = dim(fi[, 1, 2]))
#' fi[2, , 2]
#' fi[2, , 2] = array(10, dim = dim(fi[2, , 2]))
#' fi[2, 1, ]
#' # fi[2, 1, ] = array(10, dim = dim(fi[2, 1, ]))
#' fi[2, , ]
#' fi[, 1, ]
#' fi[, , 2]
#' # fi[, , 2] = array(10, dim = dim(fi[, , 2]))
#' 
#' l = prod(dim(fi))
#' sub = sample(c(TRUE, FALSE), size = l, replace = TRUE)
#' sub = array(sub, dim = dim(fi))
#' fi[sub] = rnorm(sum(sub))
setMethod(
  f = "[<-",
  signature(x = "antsImage", i = "array"),
  definition = function(x, i, j, ..., value) {
    if (typeof(i) != "logical") {
      stop("'mask' provided is not of type 'logical'")
    }
    region <-
      new("antsRegion", index = integer(), size = integer())
    return(.Call("antsImage_SetRegion", x, 
                 i, region, value, PACKAGE = "ANTsRCore"))
  }
)


#' @rdname brackets
#' @aliases [<-,antsImage,matrix-method
#' @examples
#' fi <- antsImageRead(getANTsRData( "r16" ))
#' l = prod(dim(fi))
#' sub = sample(c(TRUE, FALSE), size = l, replace = TRUE)
#' sub = array(sub, dim = dim(fi))
#' fi[sub] = rnorm(sum(sub))
setMethod(
  f = "[<-",
  signature(x = "antsImage", i = "matrix"),
  definition = function(x, i, j, ..., value) {
    if (typeof(i) != "logical") {
      stop("'mask' provided is not of type 'logical'")
    }
    region <-
      new("antsRegion", index = integer(), size = integer())
    return(.Call("antsImage_SetRegion", x, 
                 i, region, value, PACKAGE = "ANTsRCore"))
  }
)

#' @rdname brackets
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
    return(.Call("antsImage_SetRegion", x, 
                 i$mask, i$region, value, PACKAGE = "ANTsRCore"))
  }
)

#' @rdname brackets
#' @aliases [<-,antsImage,antsImage-method
#' @examples
#' fi <- antsImageRead(getANTsRData( "r16" ))
#' fi[ fi > 200 ] = 200
setMethod(
  f = "[<-",
  signature(x = "antsImage", i = "antsImage"),
  definition = function(x, i, j, ..., value) {
    if ( i@pixeltype != "unsigned char") {
      i = antsImageClone(i, out_pixeltype="unsigned char")
    }
    return(.Call("antsImage_SetByImage", 
                 x, i, value, PACKAGE = "ANTsRCore"))
  }
)


#' @rdname brackets
#' @aliases [<-,antsImage,NULL,antsRegion-method
setMethod(
  f = "[<-",
  signature(x = "antsImage", i = "NULL", j = "antsRegion"),
  definition = function(x, i, j, ..., value) {
    mask <- logical(0)
    return(.Call("antsImage_SetRegion", 
                 x, mask, j, value, PACKAGE = "ANTsRCore"))
  }
)

#' @rdname brackets
#' @aliases [<-,antsImage,logical,antsRegion-method
setMethod(
  f = "[<-",
  signature(x = "antsImage", i = "logical", j = "antsRegion"),
  definition = function(x, i, j, ..., value) {
    return(.Call("antsImage_SetRegion", 
                 x, i, j, value, PACKAGE = "ANTsRCore"))
  }
)


#' @rdname brackets
#' @aliases [<-,antsImage,array,antsRegion-method
setMethod(
  f = "[<-",
  signature(x = "antsImage", i = "array", j = "antsRegion"),
  definition = function(x, i, j, ..., value) {
    if (typeof(i) != "logical") {
      stop("'mask' provided is not of type 'logical'")
    }
    return(.Call("antsImage_SetRegion", 
                 x, i, j, value, PACKAGE = "ANTsRCore"))
  }
)

#' @rdname brackets
#' @aliases [<-,antsImage,matrix,antsRegion-method
setMethod(
  f = "[<-",
  signature(x = "antsImage", i = "matrix", j = "antsRegion"),
  definition = function(x, i, j, ..., value) {
    if (typeof(i) != "logical") {
      stop("'mask' provided is not of type 'logical'")
    }
    return(.Call("antsImage_SetRegion", 
                 x, i, j, value, PACKAGE = "ANTsRCore"))
  }
)


#' @rdname brackets
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

#' @rdname brackets
#' @aliases [<-,antsImage,numeric,numeric,numeric-method
#' @examples
#' fi <- antsImageRead(getANTsRData( "r16" ))
#' i = sample(seq(nrow(fi)), 5)
#' i = as.numeric(i)
#' j = i
#' fi[ i, j ] = 26.2
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

#' @rdname brackets
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


#' @rdname brackets
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


#' @rdname brackets
#' @aliases [<-,antsImage,numeric,missing,array-method
#' @examples
#' fi <- antsImageRead(getANTsRData( "r16" ))
#' fi[1,] = array(10, dim = dim(fi[1,]))
setMethod(
  f = "[<-",
  signature(
    x = "antsImage",
    i = "numeric",
    j = "missing",
    value = "array"
  ),
  definition = function(x, i, j, ..., value) {
    j = seq(dim(x)[2])
    temp <- antsSetPixels(x, i, j, ..., value = value)
    return(temp)
  }
)


#' @rdname brackets
#' @aliases [<-,antsImage,missing,numeric,array-method
#' @examples
#' fi <- antsImageRead(getANTsRData( "r16" ))
#' fi[,1] = array(10, dim = dim(fi[,1]))
setMethod(
  f = "[<-",
  signature(
    x = "antsImage",
    i = "missing",
    j = "numeric",
    value = "array"
  ),
  definition = function(x, i, j, ..., value) {
    i = seq(dim(x)[1])
    temp <- antsSetPixels(x, i, j, ..., value = value)
    return(temp)
  }
)
