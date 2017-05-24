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
#' @aliases Ops,antsImage,antsImage-method
setMethod("Ops", signature(e1 = "antsImage", e2 = "antsImage"),
          function(e1, e2) {
            ## either use drop_img_dim and validObject or take out both
            if (!antsImagePhysicalSpaceConsistency(x, y)) {
              stop("Images do not occupy the same physical space")
            }
            a1 = as.array(e1)
            a2 = as.array(e2)
            
            res <- callGeneric(a1, a2)
            res = as.antsImage(res, reference = e1)
            return(res)
          })

#' @rdname antsImageops
#' @aliases Ops,antsImage,numeric-method
setMethod("Ops", signature(e1 = "antsImage", e2 = "numeric"),
          function(e1, e2) {
            ## either use drop_img_dim and validObject or take out both
            a1 = as.array(e1)
            
            res <- callGeneric(a1, e2)
            res = as.antsImage(res, reference = e1)
            return(res)
          })

#' @rdname antsImageops
#' @aliases Ops,numeric,antsImage-method
setMethod("Ops", signature(e1 = "numeric", e2 = "antsImage"),
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
#' @aliases Ops,antsImage,logical-method
setMethod("Ops", signature(e1 = "antsImage", e2 = "logical"),
          function(e1, e2) {
            ## either use drop_img_dim and validObject or take out both
            a1 = as.array(e1)
            
            res <- callGeneric(a1, e2)
            res = as.antsImage(res, reference = e1)
            return(res)
          })

#' @rdname antsImageops
#' @aliases Ops,logical,antsImage-method
setMethod("Ops", signature(e1 = "logical", e2 = "antsImage"),
          function(e1, e2) {
            ## either use drop_img_dim and validObject or take out both
            a2 = as.array(e2)
            
            res <- callGeneric(e1, a2)
            res = as.antsImage(res, reference = e2)
            return(res)
          })

############################################
# Arrays
############################################
#' @rdname antsImageops
#' @aliases Ops,antsImage,array-method
setMethod("Ops", signature(e1 = "antsImage", e2 = "array"),
          function(e1, e2) {
            ## either use drop_img_dim and validObject or take out both
            a1 = as.array(e1)
            
            res <- callGeneric(a1, e2)
            res = as.antsImage(res, reference = e1)
            return(res)
          })

#' @rdname antsImageops
#' @aliases Ops,array,antsImage-method
setMethod("Ops", signature(e1 = "array", e2 = "antsImage"),
          function(e1, e2) {
            ## either use drop_img_dim and validObject or take out both
            a2 = as.array(e2)
            
            res <- callGeneric(e1, a2)
            res = as.antsImage(res, reference = e2)
            return(res)
          })

#' @title Math for antsImage Objects
#' @description Overloaded math for antsImage objects
#' @name antsImage-math
#' @rdname antsImagemath
#' @param x is an object of class \code{antsImage}.
#' @aliases Math,antsImage-method
setMethod("Math", signature(x = "antsImage"),
          function(x) {
            ## either use drop_img_dim and validObject or take out both
            a1 = as.array(x)
            res = callGeneric(a1)
            res = as.antsImage(res, reference = x)
            return(res)
          })
