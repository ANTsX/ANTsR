#' @rdname accessor-methods
#' @title Extract Image Attributes \code{pixeltype} from antsImages
#' @description Methods that act on the \code{slots} of the
#' antsImage object
#' @docType methods
#' @param object is an object of class \code{antsImage}.
#' @param value is the value to assign to the slot.
#' @export
#' @examples
#' img01 <- as.antsImage(array(1:64, c(4, 4, 4, 1)))
#' stopifnot(is.character(pixeltype(img01)))
#' pixeltype(img01) <- "float"
#' testthat::expect_error({
#'   pixeltype(img01) <- 1.5
#' })
setGeneric("pixeltype", function(object) {
  standardGeneric("pixeltype")
})

#' @rdname accessor-methods
#' @aliases pixeltype,antsImage-method
#' @export
setMethod("pixeltype", "antsImage", function(object) {
  object@"pixeltype"
})

#' @rdname accessor-methods
#' @aliases pixeltype<-
#' @export
setGeneric("pixeltype<-", function(object, value) {
  standardGeneric("pixeltype<-")
})

#' @rdname accessor-methods
#' @aliases pixeltype<-,antsImage-method
#' @export
setMethod(
  "pixeltype<-",
  signature(object = "antsImage"),
  function(object, value) {
    if ("pixeltype" %in% slotNames(object)) {
      object@"pixeltype" <- value
    } else {
      warning("pixeltype is not in slotNames of object")
    }
    return(object)
  }
)


####################################################
# Components
####################################################
#' @rdname accessor-methods
#' @export
setGeneric("components", function(object) {
  standardGeneric("components")
})

#' @rdname accessor-methods
#' @aliases components,antsImage-method
#' @export
#' @examples
#' img01 <- as.antsImage(array(1:64, c(4, 4, 4, 1)))
#' stopifnot(components(img01) == 1)
#' components(img01) <- 1L
#' testthat::expect_error({
#'   components(img01) <- 1.5
#' })
setMethod("components", "antsImage", function(object) {
  object@"components"
})

#' @rdname accessor-methods
#' @aliases components<-
#' @export
setGeneric("components<-", function(object, value) {
  standardGeneric("components<-")
})

#' @rdname accessor-methods
#' @aliases components<-,antsImage-method
#' @export
setMethod(
  "components<-",
  signature(object = "antsImage"),
  function(object, value) {
    if (as.integer(value) == value) {
      value <- as.integer(value)
    }
    if ("components" %in% slotNames(object)) {
      object@"components" <- value
    } else {
      # don't think getting to this is possible with validation
      warning("components is not in slotNames of object")
    }
    return(object)
  }
)


####################################################
# Spacing
####################################################
#' @rdname accessor-methods
#' @export
setGeneric("spacing", function(object) {
  standardGeneric("spacing")
})

#' @rdname accessor-methods
#' @aliases spacing,antsImage-method
#' @export
#' @examples
#' img01 <- as.antsImage(array(1:64, c(4, 4, 4, 1)))
#' stopifnot(all(spacing(img01) == rep(1, 4)))
#' spacing(img01) <- rep(1, 4)
setMethod("spacing", "antsImage", function(object) {
  antsGetSpacing(object)
})

#' @rdname accessor-methods
#' @aliases spacing<-
#' @export
setGeneric("spacing<-", function(object, value) {
  standardGeneric("spacing<-")
})

#' @rdname accessor-methods
#' @aliases spacing<-,antsImage-method
#' @export
setMethod(
  "spacing<-",
  signature(object = "antsImage"),
  function(object, value) {
    antsSetSpacing(object, value)
    return(object)
  }
)


####################################################
# Direction
####################################################
#' @rdname accessor-methods
#' @export
setGeneric("direction", function(object) {
  standardGeneric("direction")
})

#' @rdname accessor-methods
#' @aliases direction,antsImage-method
#' @export
#' @examples
#' img01 <- as.antsImage(array(1:64, c(4, 4, 4, 1)))
#' stopifnot(is.matrix(direction(img01)))
#' direction(img01) <- diag(4)
setMethod("direction", "antsImage", function(object) {
  antsGetDirection(object)
})

#' @rdname accessor-methods
#' @aliases direction<-
#' @export
setGeneric("direction<-", function(object, value) {
  standardGeneric("direction<-")
})

#' @rdname accessor-methods
#' @aliases direction<-,antsImage-method
#' @export
setMethod(
  "direction<-",
  signature(object = "antsImage"),
  function(object, value) {
    antsSetDirection(object, value)
    return(object)
  }
)


####################################################
# Origin
####################################################
#' @rdname accessor-methods
#' @export
setGeneric("origin", function(object) {
  standardGeneric("origin")
})

#' @rdname accessor-methods
#' @aliases origin,antsImage-method
#' @export
#' @examples
#' img01 <- as.antsImage(array(1:64, c(4, 4, 4, 1)))
#' stopifnot(all(origin(img01) == rep(0, 4)))
#' origin(img01) <- rep(0, 4)
setMethod("origin", "antsImage", function(object) {
  antsGetOrigin(object)
})

#' @rdname accessor-methods
#' @aliases origin<-
#' @export
setGeneric("origin<-", function(object, value) {
  standardGeneric("origin<-")
})

#' @rdname accessor-methods
#' @aliases origin<-,antsImage-method
#' @export
setMethod(
  "origin<-",
  signature(object = "antsImage"),
  function(object, value) {
    antsSetOrigin(object, value)
    return(object)
  }
)
