#' Overloading binary operators for antsImage Objects
#' 
#' @title Operations for antsImage Objects
#' @description Overloaded operators for antsImage objects
#' @name antsImage-operators
#' @rdname antsImageops
#' @param e1 is an object of class \code{antsImage}.
#' @param e2 is an object of class \code{antsImage}.
#' @param x is an object of class \code{antsImage}.
#' @author John Muschellli \email{muschellij2@@gmail.com}
#' @examples
#'
#' img01 <- as.antsImage(array(1:64, c(4, 4, 4, 1)))
#' img02 <- as.antsImage(array(64:1, c(4, 4, 4, 1)))
#' is.antsImage(img01 + img02)
#' is.antsImage(sqrt(2) * img01)
#' is.antsImage(img02 / pi)
#' @aliases Arith,antsImage,antsImage-method
#' @export
setMethod(
  "Arith", signature(e1 = "antsImage", e2 = "antsImage"),
  function(e1, e2) {
    ## either use drop_img_dim and validObject or take out both
    if (!antsImagePhysicalSpaceConsistency(e1, e2)) {
      stop("Images do not occupy the same physical space")
    }
    print("Generic method being called")
    a1 <- as.array(e1)
    a2 <- as.array(e2)

    res <- callGeneric(a1, a2)
    res <- as.antsImage(res, reference = e1)
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases Arith,antsImage,numeric-method
setMethod(
  "+", signature(e1 = "antsImage", e2 = "antsImage"),
  function(e1, e2) {
    ## either use drop_img_dim and validObject or take out both
    if (!antsImagePhysicalSpaceConsistency(e1, e2)) {
      stop("Images do not occupy the same physical space")
    }
    res <- ANTsRCore::antsImageArithImageImage(e1, e2, "+")
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases Arith,antsImage,numeric-method
setMethod(
  "-", signature(e1 = "antsImage", e2 = "antsImage"),
  function(e1, e2) {
    ## either use drop_img_dim and validObject or take out both
    if (!antsImagePhysicalSpaceConsistency(e1, e2)) {
      stop("Images do not occupy the same physical space")
    }
    res <- ANTsRCore::antsImageArithImageImage(e1, e2, "-")
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases Arith,antsImage,numeric-method
setMethod(
  "*", signature(e1 = "antsImage", e2 = "antsImage"),
  function(e1, e2) {
    ## either use drop_img_dim and validObject or take out both
    if (!antsImagePhysicalSpaceConsistency(e1, e2)) {
      stop("Images do not occupy the same physical space")
    }
    res <- ANTsRCore::antsImageArithImageImage(e1, e2, "*")
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases Arith,antsImage,numeric-method
setMethod(
  "/", signature(e1 = "antsImage", e2 = "antsImage"),
  function(e1, e2) {
    ## either use drop_img_dim and validObject or take out both
    if (!antsImagePhysicalSpaceConsistency(e1, e2)) {
      stop("Images do not occupy the same physical space")
    }
    res <- ANTsRCore::antsImageArithImageImage(e1, e2, "/")
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases Arith,antsImage,numeric-method
setMethod(
  "^", signature(e1 = "antsImage", e2 = "antsImage"),
  function(e1, e2) {
    ## either use drop_img_dim and validObject or take out both
    if (!antsImagePhysicalSpaceConsistency(e1, e2)) {
      stop("Images do not occupy the same physical space")
    }
    res <- ANTsRCore::antsImageArithImageImage(e1, e2, "^")
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases Arith,antsImage,numeric-method
setMethod(
  "%%", signature(e1 = "antsImage", e2 = "antsImage"),
  function(e1, e2) {
    ## either use drop_img_dim and validObject or take out both
    if (!antsImagePhysicalSpaceConsistency(e1, e2)) {
      stop("Images do not occupy the same physical space")
    }
    res <- ANTsRCore::antsImageArithImageImage(e1, e2, "%%")
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases Arith,antsImage,numeric-method
setMethod(
  "%/%", signature(e1 = "antsImage", e2 = "antsImage"),
  function(e1, e2) {
    ## either use drop_img_dim and validObject or take out both
    if (!antsImagePhysicalSpaceConsistency(e1, e2)) {
      stop("Images do not occupy the same physical space")
    }
    res <- ANTsRCore::antsImageArithImageImage(e1, e2, "%/%")
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases Arith,antsImage,numeric-method
setMethod(
  "Arith", signature(e1 = "antsImage", e2 = "numeric"),
  function(e1, e2) {
    ## either use drop_img_dim and validObject or take out both
    a1 <- as.array(e1)

    res <- callGeneric(a1, e2)
    res <- as.antsImage(res, reference = e1)
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases Arith,antsImage,numeric-method
setMethod(
  "+", signature(e1 = "antsImage", e2 = "numeric"),
  function(e1, e2) {
    res <- ANTsRCore::antsImageArithImageNumeric(e1, e2, "+")
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases Arith,antsImage,numeric-method
setMethod(
  "-", signature(e1 = "antsImage", e2 = "numeric"),
  function(e1, e2) {
    res <- ANTsRCore::antsImageArithImageNumeric(e1, e2, "-")
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases Arith,antsImage,numeric-method
setMethod(
  "*", signature(e1 = "antsImage", e2 = "numeric"),
  function(e1, e2) {
    res <- ANTsRCore::antsImageArithImageNumeric(e1, e2, "*")
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases Arith,antsImage,numeric-method
setMethod(
  "/", signature(e1 = "antsImage", e2 = "numeric"),
  function(e1, e2) {
    res <- ANTsRCore::antsImageArithImageNumeric(e1, e2, "/")
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases Arith,antsImage,numeric-method
setMethod(
  "^", signature(e1 = "antsImage", e2 = "numeric"),
  function(e1, e2) {
    res <- ANTsRCore::antsImageArithImageNumeric(e1, e2, "^")
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases Arith,antsImage,numeric-method
setMethod(
  "%%", signature(e1 = "antsImage", e2 = "numeric"),
  function(e1, e2) {
    res <- ANTsRCore::antsImageArithImageNumeric(e1, e2, "%%")
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases Arith,antsImage,numeric-method
setMethod(
  "%/%", signature(e1 = "antsImage", e2 = "numeric"),
  function(e1, e2) {
    res <- ANTsRCore::antsImageArithImageNumeric(e1, e2, "%/%")
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases Arith,antsImage,numeric-method
setMethod(
  "+", signature(e1 = "numeric", e2 = "antsImage"),
  function(e1, e2) {
    res <- ANTsRCore::antsImageArithNumericImage(e1, e2, "+")
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases Arith,antsImage,numeric-method
setMethod(
  "-", signature(e1 = "numeric", e2 = "antsImage"),
  function(e1, e2) {
    res <- ANTsRCore::antsImageArithNumericImage(e1, e2, "-")
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases Arith,antsImage,numeric-method
setMethod(
  "*", signature(e1 = "numeric", e2 = "antsImage"),
  function(e1, e2) {
    res <- ANTsRCore::antsImageArithNumericImage(e1, e2, "*")
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases Arith,antsImage,numeric-method
setMethod(
  "/", signature(e1 = "numeric", e2 = "antsImage"),
  function(e1, e2) {
    res <- ANTsRCore::antsImageArithNumericImage(e1, e2, "/")
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases Arith,antsImage,numeric-method
setMethod(
  "^", signature(e1 = "numeric", e2 = "antsImage"),
  function(e1, e2) {
    res <- ANTsRCore::antsImageArithNumericImage(e1, e2, "^")
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases Arith,antsImage,numeric-method
setMethod(
  "%%", signature(e1 = "numeric", e2 = "antsImage"),
  function(e1, e2) {
    res <- ANTsRCore::antsImageArithNumericImage(e1, e2, "%%")
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases Arith,antsImage,numeric-method
setMethod(
  "%/%", signature(e1 = "numeric", e2 = "antsImage"),
  function(e1, e2) {
    res <- ANTsRCore::antsImageArithNumericImage(e1, e2, "%/%")
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases Arith,antsImage,numeric-method
setMethod(
  "Arith", signature(e1 = "antsImage", e2 = "missing"),
  function(e1, e2) {
    ## This is for unary operators
    a1 <- as.array(e1)

    res <- callGeneric(a1)
    # keep values such as -1 representable (unsigned types would wrap)
    res <- .as_antsImage_keep_values(res, reference = e1)
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases Arith,numeric,antsImage-method
setMethod(
  "Arith", signature(e1 = "numeric", e2 = "antsImage"),
  function(e1, e2) {
    ## either use drop_img_dim and validObject or take out both
    a2 <- as.array(e2)

    res <- callGeneric(e1, a2)
    res <- as.antsImage(res, reference = e2)
    return(res)
  }
)

############################################
# Logicals
############################################
#' @rdname antsImageops
#' @aliases Arith,antsImage,logical-method
setMethod(
  "Arith", signature(e1 = "antsImage", e2 = "logical"),
  function(e1, e2) {
    e2 <- as.numeric(e2)
    res <- callGeneric(e1, e2)
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases Arith,logical,antsImage-method
setMethod(
  "Arith", signature(e1 = "logical", e2 = "antsImage"),
  function(e1, e2) {
    e1 <- as.numeric(e1)
    res <- callGeneric(e1, e2)
    return(res)
  }
)

############################################
# Arrays
############################################
#' @rdname antsImageops
#' @aliases Arith,antsImage,array-method
setMethod(
  "Arith", signature(e1 = "antsImage", e2 = "array"),
  function(e1, e2) {
    e2 <- .as_antsImage_keep_values(e2, reference = e1)
    res <- callGeneric(e1, e2)
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases Arith,array,antsImage-method
setMethod(
  "Arith", signature(e1 = "array", e2 = "antsImage"),
  function(e1, e2) {
    e1 <- .as_antsImage_keep_values(e1, reference = e2)
    res <- callGeneric(e1, e2)
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases Arith,list,antsImage-method
setMethod(
  "Arith", signature(e1 = "list", e2 = "antsImage"),
  function(e1, e2) {
    ## either use drop_img_dim and validObject or take out both
    # a2 = as.array(e1)
    stop("antsRegions not done yet!")
  }
)

#' @rdname antsImageops
#' @aliases Arith,antsImage,list-method
setMethod(
  "Arith", signature(e1 = "antsImage", e2 = "list"),
  function(e1, e2) {
    ## either use drop_img_dim and validObject or take out both
    # a2 = as.array(e1)
    stop("antsRegions not done yet!")
  }
)

# Convert an array to an antsImage sharing the header of `reference` while
# keeping the array's values: unlike as.antsImage(x, reference = reference),
# the reference pixeltype is not adopted (that truncates, e.g. 0.5 -> 0 for an
# unsigned char reference); double stays double, everything else is float.
.as_antsImage_keep_values <- function(object, reference) {
  pixeltype <- if (reference@pixeltype == "double") "double" else "float"
  ndim <- length(dim(object))
  return(ANTsRCore::antsImage_asantsImage(
    object, pixeltype,
    antsGetSpacing(reference)[seq_len(ndim)],
    antsGetOrigin(reference)[seq_len(ndim)],
    antsGetDirection(reference),
    (reference@components > 1)
  ))
}
