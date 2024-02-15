#' @title Math for antsImage Objects
#' @description Overloaded math for antsImage objects
#' @name antsImage-math
#' @rdname antsImagemath
#' @param x is an object of class \code{antsImage}.
#' @aliases Math,antsImage-method
#' @examples
#' img01 <- as.antsImage(array(1:64, c(4,4,4,1)))
#' testthat::expect_true(is.antsImage(abs(img01)))
#' testthat::expect_true(is.antsImage(sign(img01)))
#' testthat::expect_true(is.antsImage(sqrt(img01)))
#' testthat::expect_true(is.antsImage(ceiling(img01)))
#' testthat::expect_true(is.antsImage(floor(img01)))
#' testthat::expect_true(is.antsImage(trunc(img01)))
#' @export
setMethod("Math", signature(x = "antsImage"),
          function(x) {
            a1 = as.array(x)
            res = callGeneric(a1)
            res = as.antsImage(res, reference = x)
            return(res)
          })

#' @rdname antsImagemath
#' @export
setMethod("abs", signature(x = "antsImage"),
          function(x) {
            return(.Call("antsImageMath", x, "abs", PACKAGE = "ANTsRCore"))
          })

#' @rdname antsImagemath
#' @export
setMethod("sign", signature(x = "antsImage"),
          function(x) {
            return(.Call("antsImageMath", x, "sign", PACKAGE = "ANTsRCore"))
          })

#' @rdname antsImagemath
#' @export
setMethod("sqrt", signature(x = "antsImage"),
          function(x) {
            return(.Call("antsImageMath", x, "sqrt", PACKAGE = "ANTsRCore"))
          })

#' @rdname antsImagemath
#' @export
setMethod("ceiling", signature(x = "antsImage"),
          function(x) {
            return(.Call("antsImageMath", x, "ceiling", PACKAGE = "ANTsRCore"))
          })

#' @rdname antsImagemath
#' @export
setMethod("round", signature(x = "antsImage"),
          function(x) {
            return(.Call("antsImageMath", x, "round", PACKAGE = "ANTsRCore"))
          })

#' @rdname antsImagemath
#' @export
setMethod("floor", signature(x = "antsImage"),
          function(x) {
            return(.Call("antsImageMath", x, "floor", PACKAGE = "ANTsRCore"))
          })

#' @rdname antsImagemath
#' @export
setMethod("trunc", signature(x = "antsImage"),
          function(x) {
            return(.Call("antsImageMath", x, "trunc", PACKAGE = "ANTsRCore"))
          })

#' @rdname antsImagemath
#' @param base a positive or complex number: 
#' the base with respect to which logarithms are computed. 
#' Defaults to e=exp(1).
#' @export
#' @examples
#' img01 <- as.antsImage(array(1:64, c(4,4,4,1)))
#' testthat::expect_true(is.antsImage(log(img01)))
#' testthat::expect_true(is.antsImage(exp(img01)))
#' 
#' testthat::expect_true(is.antsImage(log(img01, base = exp(1)))) 
#' testthat::expect_true(is.antsImage(log(img01, base = 2))) 
#' testthat::expect_true(is.antsImage(log(img01, base = 10))) 
#' testthat::expect_true(is.antsImage(log10(img01)))
#' testthat::expect_true(is.antsImage(log2(img01)))
#' 
#' testthat::expect_true(is.antsImage(gamma(img01)))
#' testthat::expect_true(is.antsImage(lgamma(img01)))
#' 
#' testthat::expect_true(is.antsImage(cos(img01)))
#' testthat::expect_true(is.antsImage(cospi(img01)))
#' testthat::expect_true(is.antsImage(acos(img01)))
#' testthat::expect_true(is.antsImage(acosh(img01)))
#' 
#' testthat::expect_true(is.antsImage(sin(img01)))
#' testthat::expect_true(is.antsImage(sinpi(img01)))
#' testthat::expect_true(is.antsImage(asin(img01)))
#' testthat::expect_true(is.antsImage(asinh(img01)))
#' 
#' testthat::expect_true(is.antsImage(tan(img01)))
#' testthat::expect_true(is.antsImage(tanpi(img01)))
#' testthat::expect_true(is.antsImage(atan(img01)))
#' testthat::expect_true(is.antsImage(atanh(img01)))
setMethod("log", signature(x = "antsImage"),
          function(x, base=exp(1) ) {
            if ( base==exp(1) ) {
              return(.Call("antsImageMath", x, "log", PACKAGE = "ANTsRCore"))
            }
            else if ( base==2 ) {
              return(.Call("antsImageMath", x, "log2", PACKAGE = "ANTsRCore"))
            }
            else if ( base==10 ) {
              return(.Call("antsImageMath", x, "log10", PACKAGE = "ANTsRCore"))
            }
            else {
              res=.Call("antsImageMath", x, "log10", PACKAGE = "ANTsRCore")
              res = res/(log10(base))
              return(res)
            }

          })

#' @rdname antsImagemath
#' @export
setMethod("log10", signature(x = "antsImage"),
          function(x) {
            return(.Call("antsImageMath", x, "log10", PACKAGE = "ANTsRCore"))
          })

#' @rdname antsImagemath
#' @export
setMethod("log2", signature(x = "antsImage"),
          function(x) {
            return(.Call("antsImageMath", x, "log2", PACKAGE = "ANTsRCore"))
          })

#' @rdname antsImagemath
#' @export
setMethod("acos", signature(x = "antsImage"),
          function(x) {
            return(.Call("antsImageMath", x, "acos", PACKAGE = "ANTsRCore"))
          })

#' @rdname antsImagemath
#' @export
setMethod("asin", signature(x = "antsImage"),
          function(x) {
            return(.Call("antsImageMath", x, "asin", PACKAGE = "ANTsRCore"))
          })

#' @rdname antsImagemath
#' @export
setMethod("atan", signature(x = "antsImage"),
          function(x) {
            return(.Call("antsImageMath", x, "atan", PACKAGE = "ANTsRCore"))
          })

#' @rdname antsImagemath
#' @export
setMethod("acosh", signature(x = "antsImage"),
          function(x) {
            return(.Call("antsImageMath", x, "acosh", PACKAGE = "ANTsRCore"))
          })

#' @rdname antsImagemath
#' @export
setMethod("asinh", signature(x = "antsImage"),
          function(x) {
            return(.Call("antsImageMath", x, "asinh", PACKAGE = "ANTsRCore"))
          })

#' @rdname antsImagemath
#' @export
setMethod("atanh", signature(x = "antsImage"),
          function(x) {
            return(.Call("antsImageMath", x, "atanh", PACKAGE = "ANTsRCore"))
          })

#' @rdname antsImagemath
#' @export
setMethod("cos", signature(x = "antsImage"),
          function(x) {
            return(.Call("antsImageMath", x, "cos", PACKAGE = "ANTsRCore"))
          })

#' @rdname antsImagemath
#' @export
setMethod("sin", signature(x = "antsImage"),
          function(x) {
            return(.Call("antsImageMath", x, "sin", PACKAGE = "ANTsRCore"))
          })

#' @rdname antsImagemath
#' @export
setMethod("tan", signature(x = "antsImage"),
          function(x) {
            return(.Call("antsImageMath", x, "tan", PACKAGE = "ANTsRCore"))
          })

#' @rdname antsImagemath
#' @export
setMethod("cosh", signature(x = "antsImage"),
          function(x) {
            return(.Call("antsImageMath", x, "cosh", PACKAGE = "ANTsRCore"))
          })

#' @rdname antsImagemath
#' @export
setMethod("sinh", signature(x = "antsImage"),
          function(x) {
            return(.Call("antsImageMath", x, "sinh", PACKAGE = "ANTsRCore"))
          })

#' @rdname antsImagemath
#' @export
setMethod("tanh", signature(x = "antsImage"),
          function(x) {
            return(.Call("antsImageMath", x, "tanh", PACKAGE = "ANTsRCore"))
          })

#' @rdname antsImagemath
#' @export
setMethod("cospi", signature(x = "antsImage"),
          function(x) {
            return(.Call("antsImageMath", x, "cospi", PACKAGE = "ANTsRCore"))
          })

#' @rdname antsImagemath
#' @export
setMethod("sinpi", signature(x = "antsImage"),
          function(x) {
            return(.Call("antsImageMath", x, "sinpi", PACKAGE = "ANTsRCore"))
          })

#' @rdname antsImagemath
#' @export
setMethod("tanpi", signature(x = "antsImage"),
          function(x) {
            masker = x == 0.5
            suppressWarnings(check_res <- any(as.array(masker)))
            if (check_res) {
              warning(paste0("tanpi doesn't have correct behavior ", 
                             "when x = 0.5 compared to R!"))
            }
            res = .Call("antsImageMath", x, "tanpi", PACKAGE = "ANTsRCore")
            if (check_res) {
              res[masker] = NaN
            }
            return(res)
          })

#' @rdname antsImagemath
#' @export
setMethod("exp", signature(x = "antsImage"),
          function(x) {
            return(.Call("antsImageMath", x, "exp", PACKAGE = "ANTsRCore"))
          })

#' @rdname antsImagemath
#' @export
setMethod("gamma", signature(x = "antsImage"),
          function(x) {
            mask = x == 0
            res = .Call("antsImageMath", x, "gamma", PACKAGE = "ANTsRCore")
            res[mask] = NaN
            return(res)
          })

#' @rdname antsImagemath
#' @export
setMethod("lgamma", signature(x = "antsImage"),
          function(x) {
            return(.Call("antsImageMath", x, "lgamma", PACKAGE = "ANTsRCore"))
          })


# setMethod("!")

#' @rdname antsImagemath
#' @aliases !,antsImage-method
setMethod(f = "!", signature(x = "antsImage"), definition = function(x) {
  # a2 = as.array(x)
  # !a2
  x == 0
})
