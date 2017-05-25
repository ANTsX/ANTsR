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
            if (!antsImagePhysicalSpaceConsistency(e1, e2)) {
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

#' @rdname antsImageops
#' @aliases Ops,list,antsImage-method
setMethod("Ops", signature(e1 = "list", e2 = "antsImage"),
          function(e1, e2) {
            ## either use drop_img_dim and validObject or take out both
            # a2 = as.array(e1)
            stop("antsRegions not done yet!")
          })

#' @rdname antsImageops
#' @aliases Ops,antsImage,list-method
setMethod("Ops", signature(e1 = "antsImage", e2 = "list"),
          function(e1, e2) {
            ## either use drop_img_dim and validObject or take out both
            # a2 = as.array(e1)
            stop("antsRegions not done yet!")
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

#' @title Summary for antsImage Objects
#' @description Overloaded Summary for antsImage objects
#' @name antsImage-summary
#' @rdname antsImageSummary
#' @param x is an object of class \code{antsImage}.
#' @param ... further arguments passed to summary methods
#' @param na.rm logical: should missing values be removed?
#' @aliases Summary,antsImage-method
setMethod("Summary", signature(x = "antsImage"),
          function(x, ..., na.rm = FALSE) {
            ## either use drop_img_dim and validObject or take out both
            a1 = as.array(x)
            res = callGeneric(a1, ..., na.rm = na.rm)
            # res = as.antsImage(res, reference = x)
            return(res)
          })

#' @rdname antsImagemath
#' @aliases !,antsImage-method
setMethod(f = "!", signature(x = "antsImage"), definition = function(x) {
  a2 = as.array(x)
  !a2
})

#' #' @rdname antsImagemath
#' #' @aliases xor,antsImage-method
#' setMethod(f = "xor", signature(x = "antsImage",
#'                              y = "antsImage"), definition = function(x) {
#'   xx = as.array(x)
#'   yy = as.array(y)
#'   xor(xx, yy)
#'   !a2
#' })


#' @title Mean for antsImage Objects
#' @description Overloaded Mean for antsImage objects
#' @param x is an object of class \code{antsImage}.
#' @aliases Summary,antsImage-method
#' @rdname antsImageSummary
#' @export
mean.antsImage = function(x, ...) {
  x = as.array(x)
  # if (missing(mask)) {
  #   x = img_data(x)
  #   x = c(x)
  # } else {
  #   x = mask_vals(object = x, mask)
  # }
  mean(x, ...)
}


#' @rdname antsImageSummary
#' @export
sd.antsImage = function(x, ...) {
  x = as.array(x)
  # if (missing(mask)) {
  #   x = img_data(x)
  #   x = c(x)
  # } else {
  #   x = mask_vals(object = x, mask)
  # }
  sd(x, ...)
}

#' @rdname var
#' @param x antsImage object
#' @param y antsImage object, but likely null
#' @param ... additional arguments passed to \code{\link{var}}
#' 
#' @export
setGeneric("var", function(x, y = NULL, ...) { 
  standardGeneric("var") 
}
)

#' @rdname var
#' @aliases var,antsImage-method
setMethod("var", signature(x = "antsImage"),
          function(x, y = NULL, ...) {
            ## either use drop_img_dim and validObject or take out both
            x = as.array(x)
            if (!is.null(y)) {
              y = as.array(y)
            }
            var(x = x, y = y,  ...)
          })
