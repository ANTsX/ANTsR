# From http://r.789695.n4.nabble.com/trouble-with-S4-methods-for-
# group-quot-Summary-quot-td790541.html
.max_def <-
  function(x, ..., na.rm = FALSE)
    base::max(x, ..., na.rm = na.rm)
.min_def <-
  function(x, ..., na.rm = FALSE)
    base::min(x, ..., na.rm = na.rm)
.range_def <-
  function(x, ..., na.rm = FALSE)
    base::range(x, ..., na.rm = na.rm)
.prod_def <-
  function(x, ..., na.rm = FALSE)
    base::prod(x, ..., na.rm = na.rm)
.sum_def <-
  function(x, ..., na.rm = FALSE)
    base::sum(x, ..., na.rm = na.rm)
.any_def <-
  function(x, ..., na.rm = FALSE)
    base::any(x, ..., na.rm = na.rm)
.all_def <-
  function(x, ..., na.rm = FALSE)
    base::all(x, ..., na.rm = na.rm)

#' @title Summary for antsImage Objects
#' @description Overloaded Summary for antsImage objects
#' @name antsImage-summary
#' @rdname antsImageSummary
#' @param x is an object of class \code{antsImage}.
#' @param ... further arguments passed to summary methods
#' @param na.rm logical: should missing values be removed?
#' @param mask binary mask of values to subset
#' @examples
#' vec = 1:64
#' img01 <- as.antsImage(array(vec, c(4,4,4,1)))
#' testthat::expect_equal(max(img01), max(vec))
#' max(img01)
#' testthat::expect_equal(min(img01), min(vec))
#' min(img01)
#' testthat::expect_equal(sum(img01), sum(vec))
#' range(img01)
#' testthat::expect_equal(range(img01), range(vec))
#' prod(img01/25)
#' testthat::expect_equal(prod(img01/25), prod(vec/25), tolerance = 1e-5)
#' @aliases Summary,antsImage-method
#' @export
setMethod("Summary", "antsImage",
          function(x, ..., mask = NULL, na.rm = FALSE) {
            args = list(...)
            # mask = args$mask
            # args$mask = NULL
            x = mask_values(x, mask)
            # I think this makes sense but should ask Avants.
            # relevant for warnings for all/any in summary
            # if (all(x %in% c(0, 1, NA, NaN))) {
            #   x = as.logical(x)
            # }
            args$x = x
            args$na.rm = na.rm


            res = do.call(callGeneric, args = args)
            # L = list(...)
            # mask = L$mask
            # rm(list = "L"); gc();
            # x = mask_values(x, mask)
            # res = callGeneric(x, ..., na.rm = na.rm)
            # # res = as.antsImage(res, reference = x)
            return(res)
          })

#' @rdname antsImageSummary
#' @export
setGeneric("max", function(x, ..., na.rm = FALSE) 
  standardGeneric("max"),
  useAsDefault = .max_def, group = "Summary")

#' @rdname antsImageSummary
#' @export
setGeneric("min", function(x, ..., na.rm = FALSE) 
  standardGeneric("min"),
  useAsDefault = .min_def, group = "Summary")

#' @rdname antsImageSummary
#' @export
setMethod("min", "antsImage",
          function(x, ..., mask = NULL, na.rm = FALSE) {
  return(drop(antsImageStats(x,mask,na.rm)$min))
})

#' @rdname antsImageSummary
#' @export
setMethod("max", "antsImage",
          function(x, ..., mask = NULL, na.rm = FALSE) {
            return(drop(antsImageStats(x,mask,na.rm)$max))
          })

#' @rdname antsImageSummary
#' @export
setGeneric("range", function(x, ..., na.rm = FALSE) 
  standardGeneric("range"),
  useAsDefault = .range_def, group = "Summary")

#' @rdname antsImageSummary
#' @export
setGeneric("prod", function(x, ..., na.rm = FALSE) 
  standardGeneric("prod"),
  useAsDefault = .prod_def, group = "Summary")

#' @rdname antsImageSummary
#' @export
setGeneric("sum", function(x, ..., na.rm = FALSE) 
  standardGeneric("sum"),
  useAsDefault = .sum_def, group = "Summary")

#' @rdname antsImageSummary
#' @export
setGeneric("any", function(x, ..., na.rm = FALSE) 
  standardGeneric("any"),
  useAsDefault = .any_def, group = "Summary")

#' @rdname antsImageSummary
#' @export
setGeneric("all", function(x, ..., na.rm = FALSE) 
  standardGeneric("all"),
  useAsDefault = .all_def, group = "Summary")


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
#' @param na.rm Remove missing values
#' @param ... additional arguments to pass to \code{\link{mean}}
#' @param mask binary mask of values to subset
#' @rdname mean
#' @export
#' @examples
#' img =  makeImage(c(10, 10, 10), rnorm(1000))
#' mean(img)
#' mean(img, na.rm = TRUE)
#' mean(img > 0)
#' mean(img > 0, na.rm = TRUE)
#' mean(img, mask = img > 0)
#' mean(img, mask = img > 0,  na.rm = TRUE)
#' arr = as.array(img)
#' arr[1,1,1] = NA
#' img = as.antsImage(arr, reference = img)
#' m = mean(img)
#' stopifnot(is.nan(m))
#' mean(img, na.rm = TRUE)
#' m = mean(0 < img)
#' stopifnot(is.nan(m))
#' m = mean(img > 0)
#' # stopifnot(is.nan(m))
#' mean(img > 0, na.rm = TRUE)
#' m = mean(img, mask = img > 0)
#' stopifnot(is.nan(m))
#' mean(img, mask = img > 0, na.rm = TRUE)
mean.antsImage = function(x, ..., mask=NULL, na.rm=FALSE) {
  return(drop(antsImageStats(x,mask,na.rm)$mean))
}


#' @title Median for antsImage Objects
#' @description Overloaded Median for antsImage objects
#' @param x is an object of class \code{antsImage}.
#' @param na.rm a logical value indicating whether NA should be removed
#' @param mask is an object of class \code{antsImage}
#' @param ... additional arguments to send to \code{median}
#' @rdname median
#' @export
#' @importFrom stats median
median.antsImage = function(x, na.rm = FALSE, ..., mask = NULL) {
  args = list(...)
  # mask = args$mask
  x = mask_values(x, mask)
  # args$mask = NULL
  args$x = x
  args$na.rm = na.rm
  do.call("median", args)
  # median(x = x, na.rm = na.rm)
}

#' @title Unique for antsImage Objects
#' @description Overloaded uniqueness operation for antsImage objects
#' @param x is an object of class \code{antsImage}.
#' @param incomparables a vector of values that cannot be compared.
#' @param ... additional arguments passed to \code{\link{unique}}
#' @param mask binary mask of values to subset
#' @rdname unique
#'
#' @examples
#' img <- antsImageRead( getANTsRData( "r16" ) )
#' img[img > 5] = 0
#' sort(unique(img))
#'
#' @export
unique.antsImage = function(x, incomparables = FALSE, mask = NULL, ...) {
  x = mask_values(x, mask)
  unique(x, incomparables = incomparables, ...)
}

#' @rdname sd
#' @title SD generic
#' @description Calculates the SD of an image
#'
#' @param x an object for which we want to compute the SD
#' @export
sd = function(x, na.rm = FALSE, ...){
  UseMethod("sd")
}

#' @rdname sd
#' @export
#' @importFrom stats sd
sd.default = function(x, na.rm = FALSE, ...){
  stats::sd(x, na.rm = FALSE)
}

#' @rdname sd
#' @title SD for antsImage Objects
#' @description Overloaded SD for antsImage objects
#' @param na.rm a logical value indicating whether NA should be removed
#' @param mask is an object of class \code{antsImage}
#' @param \dots Any additional arguments to be passed to 
#' \code{\link[stats]{sd}}
#' 
#' @export
#' @importFrom stats sd
#' @examples
#' img <- antsImageRead( getANTsRData( "r16" ) )
#' sd(img)
#' sd(img, mask  = img > 0 )
sd.antsImage = function(x, na.rm = FALSE, ..., mask=NULL) {
  # print("sd.antsImage")
  #args = list(...)
  #mask = args$mask
  #args$mask = NULL
  #x = mask_values(x, mask)
  #args$x = x
  #do.call(sd, args = args)
  # sd(x, ...)
  return( drop(antsImageStats(x,mask,na.rm)$sd ))
}

#' @rdname var
#' @title Variance generic
#' @description Calculates the variance of an image
#'
#' @param x an object for which we want to compute the variance
#' @param \dots Any additional arguments to be passed to 
#' \code{\link[stats]{var}}
#' @export
var = function(x, ...){
  UseMethod("var")
}

#' @rdname var
#' @export
#' @importFrom stats var
var.default = function(x, ...){
  stats::var(x, ...)
}

#' @rdname var
#' @export
#' @param na.rm a logical value indicating whether NA should be removed
#' @param mask is an object of class \code{antsImage}
#' @examples
#' img <- antsImageRead( getANTsRData( "r16" ) )
#' var(img)
#' @method var antsImage
var.antsImage = function(x, ...,  na.rm=FALSE, mask=NULL) {
  #args = list(...)
  #mask = args$mask
  #args$mask = NULL
  #x = mask_values(x, mask)
  #args$x = x
  #do.call(var, args = args)
  drop(antsImageStats(x,mask,na.rm)$variance)
}

#' @rdname antsImageops
#' @aliases Math,antsImage,numeric-method
#setMethod("abs", signature(e1 = "antsImage"),
#          function(e1) {
#            res = .Call("antsImageMath",
#                        e1, "abs", PACKAGE = "ANTsRCore")
#            return(res)
#          })
