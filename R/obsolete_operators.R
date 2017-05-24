#' @describeIn antsImage
#' @aliases min,antsImage-method
setMethod(f = "min", signature(x = "antsImage"), definition = function(x) {
  return(min(as.array(x)))
})

#' @describeIn antsImage
#' @aliases max,antsImage-method
setMethod(f = "max", signature(x = "antsImage"), definition = function(x) {
  return(max(as.array(x)))
})

#' @describeIn antsImage
#' @aliases var,antsImage-method
setMethod(f = "var", signature(x = "antsImage"), definition = function(x) {
  return(var(as.vector(as.array(x))))
})

#' @describeIn antsImage
#' @aliases range,antsImage-method
setMethod(f = "range", signature(x = "antsImage"), definition = function(x) {
  return(range(as.array(x)))
})

#' @describeIn antsImage
#' @aliases sd,antsImage-method
setMethod(f = "sd", signature(x = "antsImage"), definition = function(x) {
  return(sd(as.array(x)))
})



#' @rdname antsImageArith
#' @aliases mean,antsImage-method
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

#' @rdname antsImageArith
#' @aliases sum,antsImage-method
setMethod(f = "sum", signature(x = "antsImage"), definition = function(x, mask = logical()) {
  if (typeof(mask) != "logical") {
    stop("'mask' provided is not of type 'logical'")
  }
  
  if (length(mask) == 0) {
    return(sum(as.array(x)))
  } else {
    return(sum(as.array(x)[mask]))
  }
})


#' @describeIn antsImage
#' @aliases ==,antsImage-method
setMethod(f = "==", signature(e1 = "antsImage"), definition = function(e1, e2) {
  operator <- "=="
  if (class(e2) == "list") {
    if (length(e2$value) != 1) {
      stop("length of value must be 1")
    }
    if (class(e2$region) != "antsRegion") {
      stop("region argument not of class 'antsRegion'")
    }
    x = .Call("antsImage_RelationalOperators", e1, e2$value, e2$region, operator,
              PACKAGE = "ANTsR")
    return( array(dim=dim(e1), data=x) )
  } else if ((class(e2) == "numeric" | class(e2) == "integer") && length(e2) == 1) {
    if(class(e2) == "integer")
      e2 <- as.numeric(e2)
    region <- new("antsRegion", index = integer(), size = integer())
    x = .Call("antsImage_RelationalOperators", e1, e2, region,
              operator, PACKAGE = "ANTsR")
    return( array(dim=dim(e1), data=x))
  } else {
    stop("rhs must be a scalar or a list( <scalar> , <antsRegion> )")
  }
})


#' @describeIn antsImage
#' @aliases !=,antsImage-method
setMethod(f = "!=", signature(e1 = "antsImage"), definition = function(e1, e2) {
  operator <- "!="
  if (class(e2) == "list") {
    if (length(e2$value) != 1) {
      stop("length of value must be 1")
    }
    if (class(e2$region) != "antsRegion") {
      stop("region argument not of class 'antsRegion'")
    }
    x = .Call("antsImage_RelationalOperators", e1, e2$value, e2$region, operator,
              PACKAGE = "ANTsR")
    return( array(dim=dim(e1), data=x) )
  } else if (class(e2) == "numeric" && length(e2) == 1) {
    region <- new("antsRegion", index = integer(), size = integer())
    x = .Call("antsImage_RelationalOperators", e1, e2, region, operator, PACKAGE = "ANTsR")
    return( array(dim=dim(e1), data=x))
  } else {
    stop("rhs must be a scalar or a list( <scalar> , <antsRegion> )")
  }
})

#' @describeIn antsImage
#' @aliases <=,antsImage-method
setMethod(f = "<=", signature(e1 = "antsImage"), definition = function(e1, e2) {
  operator <- "<="
  if (class(e2) == "list") {
    if (length(e2$value) != 1) {
      stop("length of value must be 1")
    }
    if (class(e2$region) != "antsRegion") {
      stop("region argument not of class 'antsRegion'")
    }
    x = .Call("antsImage_RelationalOperators", e1, e2$value, e2$region, operator,
              PACKAGE = "ANTsR")
    return( array(dim=dim(e1), data=x))
  } else if (class(e2) == "numeric" && length(e2) == 1) {
    region <- new("antsRegion", index = integer(), size = integer())
    x = .Call("antsImage_RelationalOperators", e1, e2, region, operator, PACKAGE = "ANTsR")
    return( array(dim=dim(e1), data=x))
  } else {
    stop("rhs must be a scalar or a list( <scalar> , <antsRegion> )")
  }
})

#' @describeIn antsImage
#' @aliases >=,antsImage-method
setMethod(f = ">=", signature(e1 = "antsImage"), definition = function(e1, e2) {
  operator <- ">="
  if (class(e2) == "list") {
    if (length(e2$value) != 1) {
      stop("length of value must be 1")
    }
    if (class(e2$region) != "antsRegion") {
      stop("region argument not of class 'antsRegion'")
    }
    x = .Call("antsImage_RelationalOperators", e1, e2$value, e2$region, operator,
              PACKAGE = "ANTsR")
    return( array(dim=dim(e1), data=x))
  } else if (class(e2) == "numeric" && length(e2) == 1) {
    region <- new("antsRegion", index = integer(), size = integer())
    x = .Call("antsImage_RelationalOperators", e1, e2, region, operator, PACKAGE = "ANTsR")
    return( array(dim=dim(e1), data=x))
  } else {
    stop("rhs must be a scalar or a list( <scalar> , <antsRegion> )")
  }
})

#' @describeIn antsImage
#' @aliases <,antsImage-method
setMethod(f = "<", signature(e1 = "antsImage"), definition = function(e1, e2) {
  operator <- "<"
  if (class(e2) == "list") {
    if (length(e2$value) != 1) {
      stop("length of value must be 1")
    }
    if (class(e2$region) != "antsRegion") {
      stop("region argument not of class 'antsRegion'")
    }
    x=.Call("antsImage_RelationalOperators", e1, e2$value, e2$region, operator,
            PACKAGE = "ANTsR")
    return(array(dim=dim(e1), data=x))
  } else if (class(e2) == "numeric" && length(e2) == 1) {
    region <- new("antsRegion", index = integer(), size = integer())
    x=.Call("antsImage_RelationalOperators", e1, e2, region, operator, PACKAGE = "ANTsR")
    return(array(dim=dim(e1), data=x))
  } else {
    stop("rhs must be a scalar or a list( <scalar> , <antsRegion> )")
  }
})

#' @describeIn antsImage
#' @aliases >,antsImage-method
setMethod(f = ">", signature(e1 = "antsImage"), definition = function(e1, e2) {
  operator <- ">"
  if (class(e2) == "list") {
    if (length(e2$value) != 1) {
      stop("length of value must be 1")
    }
    if (class(e2$region) != "antsRegion") {
      stop("region argument not of class 'antsRegion'")
    }
    x =.Call("antsImage_RelationalOperators", e1, e2$value, e2$region, operator,
             PACKAGE = "ANTsR")
    return( array(dim=dim(e1), data=x))
  } else if (class(e2) == "numeric" && length(e2) == 1) {
    region <- new("antsRegion", index = integer(), size = integer())
    x=.Call("antsImage_RelationalOperators", e1, e2, region, operator, PACKAGE = "ANTsR")
    return(array(dim=dim(e1), data=x))
  } else {
    stop("rhs must be a scalar or a list( <scalar> , <antsRegion> )")
  }
})
