#' @rdname antsImageops
#' @aliases is.na,antsImage-method
#' @export
setMethod("is.na", signature(x = "antsImage"),
          function(x) {
            x = as.antsImage(is.na(as.array(x)), reference = x) 
            x != 0 
          }
)

#' @rdname antsImageops
#' @aliases anyNA,antsImage-method
#' @param recursive not used
#' @export
setMethod("anyNA", signature(x = "antsImage"),
          function(x, recursive = FALSE) {
            anyNA(as.array(x), recursive = recursive)
          }
)


#' @rdname antsImageops
#' @aliases is.nan,antsImage-method
#' @export
setMethod("is.nan", signature(x = "antsImage"),
          function(x) {
            x = as.antsImage(is.nan(as.array(x)), reference = x) 
            x != 0         
          }
)


#' @rdname antsImageops
#' @aliases is.infinite,antsImage-method
#' @export
setMethod("is.infinite", signature(x = "antsImage"),
          function(x) {
            x = as.antsImage(is.infinite(as.array(x)), reference = x) 
            x != 0         
          }
)


#' @rdname antsImageops
#' @aliases is.finite,antsImage-method
#' @export
setMethod("is.finite", signature(x = "antsImage"),
          function(x) {
            x = as.antsImage(is.finite(as.array(x)), reference = x) 
            x != 0  
          }
)



