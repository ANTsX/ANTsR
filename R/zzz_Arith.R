## Overloading binary operators for antsImage Objects
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @title Operations for antsImage Objects
#' @description Overloaded operators for antsImage objects
#' @name antsImage-operators
#' @rdname antsImageops
#' @param e1 is an object of class \code{antsImage}.
#' @param e2 is an object of class \code{antsImage}.
#' @author John Muschellli \email{muschellij2@@gmail.com}
#' @examples
#'
#' img01 <- as.antsImage(array(1:64, c(4,4,4,1)))
#' img02 <- as.antsImage(array(64:1, c(4,4,4,1)))
#' is.antsImage(img01 + img02)
#' is.antsImage(sqrt(2) * img01)
#' is.antsImage(img02 / pi)
#' @aliases Arith,antsImage,antsImage-method
#' @export
setMethod("Arith", signature(e1 = "antsImage", e2 = "antsImage"),
          function(e1, e2) {
            ## either use drop_img_dim and validObject or take out both
            if (!antsImagePhysicalSpaceConsistency(e1, e2)) {
              stop("Images do not occupy the same physical space")
            }
            print("Generic method being called")
            a1 = as.array(e1)
            a2 = as.array(e2)

            res <- callGeneric(a1, a2)
            res = as.antsImage(res, reference = e1)
            return(res)
          })

#' @rdname antsImageops
#' @aliases Arith,antsImage,numeric-method
setMethod("+", signature(e1 = "antsImage", e2 = "antsImage"),
          function(e1, e2) {
            ## either use drop_img_dim and validObject or take out both
            if (!antsImagePhysicalSpaceConsistency(e1, e2)) {
              stop("Images do not occupy the same physical space")
            }
            res = .Call("antsImageArithImageImage",
                        e1, e2, "+", PACKAGE = "ANTsRCore")
            return(res)
          })

#' @rdname antsImageops
#' @aliases Arith,antsImage,numeric-method
setMethod("-", signature(e1 = "antsImage", e2 = "antsImage"),
          function(e1, e2) {
            ## either use drop_img_dim and validObject or take out both
            if (!antsImagePhysicalSpaceConsistency(e1, e2)) {
              stop("Images do not occupy the same physical space")
            }
            res = .Call("antsImageArithImageImage",
                        e1, e2, "-", PACKAGE = "ANTsRCore")
            return(res)
          })

#' @rdname antsImageops
#' @aliases Arith,antsImage,numeric-method
setMethod("*", signature(e1 = "antsImage", e2 = "antsImage"),
          function(e1, e2) {
            ## either use drop_img_dim and validObject or take out both
            if (!antsImagePhysicalSpaceConsistency(e1, e2)) {
              stop("Images do not occupy the same physical space")
            }
            res = .Call("antsImageArithImageImage",
                        e1, e2, "*", PACKAGE = "ANTsRCore")
            return(res)
          })

#' @rdname antsImageops
#' @aliases Arith,antsImage,numeric-method
setMethod("/", signature(e1 = "antsImage", e2 = "antsImage"),
          function(e1, e2) {
            ## either use drop_img_dim and validObject or take out both
            if (!antsImagePhysicalSpaceConsistency(e1, e2)) {
              stop("Images do not occupy the same physical space")
            }
            res = .Call("antsImageArithImageImage",
                        e1, e2, "/", PACKAGE = "ANTsRCore")
            return(res)
          })

#' @rdname antsImageops
#' @aliases Arith,antsImage,numeric-method
setMethod("^", signature(e1 = "antsImage", e2 = "antsImage"),
          function(e1, e2) {
            ## either use drop_img_dim and validObject or take out both
            if (!antsImagePhysicalSpaceConsistency(e1, e2)) {
              stop("Images do not occupy the same physical space")
            }
            res = .Call("antsImageArithImageImage",
                        e1, e2, "^", PACKAGE = "ANTsRCore")
            return(res)
          })

#' @rdname antsImageops
#' @aliases Arith,antsImage,numeric-method
setMethod("%%", signature(e1 = "antsImage", e2 = "antsImage"),
          function(e1, e2) {
            ## either use drop_img_dim and validObject or take out both
            if (!antsImagePhysicalSpaceConsistency(e1, e2)) {
              stop("Images do not occupy the same physical space")
            }
            res = .Call("antsImageArithImageImage",
                        e1, e2, "%%", PACKAGE = "ANTsRCore")
            return(res)
          })

#' @rdname antsImageops
#' @aliases Arith,antsImage,numeric-method
setMethod("%/%", signature(e1 = "antsImage", e2 = "antsImage"),
          function(e1, e2) {
            ## either use drop_img_dim and validObject or take out both
            if (!antsImagePhysicalSpaceConsistency(e1, e2)) {
              stop("Images do not occupy the same physical space")
            }
            res = .Call("antsImageArithImageImage",
                        e1, e2, "%/%", PACKAGE = "ANTsRCore")
            return(res)
          })

#' @rdname antsImageops
#' @aliases Arith,antsImage,numeric-method
setMethod("Arith", signature(e1 = "antsImage", e2 = "numeric"),
          function(e1, e2) {
            ## either use drop_img_dim and validObject or take out both
            a1 = as.array(e1)

            res <- callGeneric(a1, e2)
            res = as.antsImage(res, reference = e1)
            return(res)
          })

#' @rdname antsImageops
#' @aliases Arith,antsImage,numeric-method
setMethod("+", signature(e1 = "antsImage", e2 = "numeric"),
          function(e1, e2) {
            res = .Call("antsImageArithImageNumeric",
                        e1, e2, "+", PACKAGE = "ANTsRCore")
            return(res)
          })

#' @rdname antsImageops
#' @aliases Arith,antsImage,numeric-method
setMethod("-", signature(e1 = "antsImage", e2 = "numeric"),
          function(e1, e2) {
            res = .Call("antsImageArithImageNumeric",
                        e1, e2, "-", PACKAGE = "ANTsRCore")
            return(res)
          })

#' @rdname antsImageops
#' @aliases Arith,antsImage,numeric-method
setMethod("*", signature(e1 = "antsImage", e2 = "numeric"),
          function(e1, e2) {
            res = .Call("antsImageArithImageNumeric",
                        e1, e2, "*", PACKAGE = "ANTsRCore")
            return(res)
          })

#' @rdname antsImageops
#' @aliases Arith,antsImage,numeric-method
setMethod("/", signature(e1 = "antsImage", e2 = "numeric"),
          function(e1, e2) {
            res = .Call("antsImageArithImageNumeric",
                        e1, e2, "/", PACKAGE = "ANTsRCore")
            return(res)
          })

#' @rdname antsImageops
#' @aliases Arith,antsImage,numeric-method
setMethod("^", signature(e1 = "antsImage", e2 = "numeric"),
          function(e1, e2) {
            res = .Call("antsImageArithImageNumeric",
                        e1, e2, "^", PACKAGE = "ANTsRCore")
            return(res)
          })

#' @rdname antsImageops
#' @aliases Arith,antsImage,numeric-method
setMethod("%%", signature(e1 = "antsImage", e2 = "numeric"),
          function(e1, e2) {
            res = .Call("antsImageArithImageNumeric",
                        e1, e2, "%%", PACKAGE = "ANTsRCore")
            return(res)
          })

#' @rdname antsImageops
#' @aliases Arith,antsImage,numeric-method
setMethod("%/%", signature(e1 = "antsImage", e2 = "numeric"),
          function(e1, e2) {
            res = .Call("antsImageArithImageNumeric",
                        e1, e2, "%/%", PACKAGE = "ANTsRCore")
            return(res)
          })

#' @rdname antsImageops
#' @aliases Arith,antsImage,numeric-method
setMethod("+", signature(e1 = "numeric", e2 = "antsImage"),
          function(e1, e2) {
            res = .Call("antsImageArithNumericImage",
                        e1, e2, "+", PACKAGE = "ANTsRCore")
            return(res)
          })

#' @rdname antsImageops
#' @aliases Arith,antsImage,numeric-method
setMethod("-", signature(e1 = "numeric", e2 = "antsImage"),
          function(e1, e2) {
            res = .Call("antsImageArithNumericImage",
                        e1, e2, "-", PACKAGE = "ANTsRCore")
            return(res)
          })

#' @rdname antsImageops
#' @aliases Arith,antsImage,numeric-method
setMethod("*", signature(e1 = "numeric", e2 = "antsImage"),
          function(e1, e2) {
            res = .Call("antsImageArithNumericImage",
                        e1, e2, "*", PACKAGE = "ANTsRCore")
            return(res)
          })

#' @rdname antsImageops
#' @aliases Arith,antsImage,numeric-method
setMethod("/", signature(e1 = "numeric", e2 = "antsImage"),
          function(e1, e2) {
            res = .Call("antsImageArithNumericImage",
                        e1, e2, "/", PACKAGE = "ANTsRCore")
            return(res)
          })

#' @rdname antsImageops
#' @aliases Arith,antsImage,numeric-method
setMethod("^", signature(e1 = "numeric", e2 = "antsImage"),
          function(e1, e2) {
            res = .Call("antsImageArithNumericImage",
                        e1, e2, "^", PACKAGE = "ANTsRCore")
            return(res)
          })

#' @rdname antsImageops
#' @aliases Arith,antsImage,numeric-method
setMethod("%%", signature(e1 = "numeric", e2 = "antsImage"),
          function(e1, e2) {
            res = .Call("antsImageArithNumericImage",
                        e1, e2, "%%", PACKAGE = "ANTsRCore")
            return(res)
          })

#' @rdname antsImageops
#' @aliases Arith,antsImage,numeric-method
setMethod("%/%", signature(e1 = "numeric", e2 = "antsImage"),
          function(e1, e2) {
            res = .Call("antsImageArithNumericImage",
                        e1, e2, "%/%", PACKAGE = "ANTsRCore")
            return(res)
          })

#' @rdname antsImageops
#' @aliases Arith,antsImage,numeric-method
setMethod("Arith", signature(e1 = "antsImage", e2 = "missing"),
          function(e1, e2) {
            ## This is for unary operators
            a1 = as.array(e1)

            res <- callGeneric(a1)
            res = as.antsImage(res, reference = e1)
            return(res)
          })

#' @rdname antsImageops
#' @aliases Arith,numeric,antsImage-method
setMethod("Arith", signature(e1 = "numeric", e2 = "antsImage"),
          function(e1, e2) {
            ## either use drop_img_dim and validObject or take out both
            a2 = as.array(e2)

            res <- callGeneric(e1, a2)
            res = as.antsImage(res, reference = e2)
            return(res)
          })

############################################
# Logicals
############################################
#' @rdname antsImageops
#' @aliases Arith,antsImage,logical-method
setMethod("Arith", signature(e1 = "antsImage", e2 = "logical"),
          function(e1, e2) {
            e2 = as.numeric(e2)
            res <- callGeneric(e1, e2)
            return(res)
          })

#' @rdname antsImageops
#' @aliases Arith,logical,antsImage-method
setMethod("Arith", signature(e1 = "logical", e2 = "antsImage"),
          function(e1, e2) {
            e1 = as.numeric(e1)
            res = callGeneric(e1, e2)
            return(res)
          })

############################################
# Arrays
############################################
#' @rdname antsImageops
#' @aliases Arith,antsImage,array-method
setMethod("Arith", signature(e1 = "antsImage", e2 = "array"),
          function(e1, e2) {
            e2 = as.antsImage(e2, reference = e1)
            res = callGeneric(e1, e2)
            return(res)
          })

#' @rdname antsImageops
#' @aliases Arith,array,antsImage-method
setMethod("Arith", signature(e1 = "array", e2 = "antsImage"),
          function(e1, e2) {
            e1 = as.antsImage(e1, reference = e2)
            res = callGeneric(e1, e2)
            return(res)
          })

#' @rdname antsImageops
#' @aliases Arith,list,antsImage-method
setMethod("Arith", signature(e1 = "list", e2 = "antsImage"),
          function(e1, e2) {
            ## either use drop_img_dim and validObject or take out both
            # a2 = as.array(e1)
            stop("antsRegions not done yet!")
          })

#' @rdname antsImageops
#' @aliases Arith,antsImage,list-method
setMethod("Arith", signature(e1 = "antsImage", e2 = "list"),
          function(e1, e2) {
            ## either use drop_img_dim and validObject or take out both
            # a2 = as.array(e1)
            stop("antsRegions not done yet!")
          })
