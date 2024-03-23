#' @rdname antsImageops
#' @aliases Logic,antsImage,antsImage-method
#' @examples
#' img01 <- as.antsImage(array(1:64, c(4, 4, 4, 1)))
#' mask1 <- img01 == 1
#' mask2 <- img01 == 2
#' testthat::expect_true(is.antsImage(mask1))
#' testthat::expect_true(is.antsImage(mask2))
#' testthat::expect_true(is.antsImage(mask1 & mask2))
#' testthat::expect_true(is.antsImage(mask1 | mask2))
#' testthat::expect_true(is.antsImage(as.array(mask2) | mask1))
#' testthat::expect_true(is.antsImage(as.array(mask2) & mask1))
#' testthat::expect_true(is.antsImage(mask2 | as.array(mask1)))
#' testthat::expect_true(is.antsImage(mask2 & as.array(mask1)))
#'
#' testthat::expect_true(is.antsImage(mask2 | 1))
#' testthat::expect_true(is.antsImage(mask2 | TRUE))
#' testthat::expect_true(is.antsImage(mask2 & 1))
#' testthat::expect_true(is.antsImage(mask2 & TRUE))

#' @export
setMethod(
  "&", signature(e1 = "antsImage", e2 = "antsImage"),
  function(e1, e2) {
    ## either use drop_img_dim and validObject or take out both
    if (!antsImagePhysicalSpaceConsistency(e1, e2)) {
      stop("Images do not occupy the same physical space")
    }
    res <- NA

    if ((e1@pixeltype == "float") |
      (e1@pixeltype == "double") |
      (e2@pixeltype == "float") |
      (e2@pixeltype == "double")) {
      res <- (e1 != 0) & (e2 != 0)
    } else {
      res <- ANTsRCore::antsImageLogicImageImage(e1, e2, "&")
    }
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases Logic,antsImage,antsImage-method
#' @export
setMethod(
  "|", signature(e1 = "antsImage", e2 = "antsImage"),
  function(e1, e2) {
    ## either use drop_img_dim and validObject or take out both
    if (!antsImagePhysicalSpaceConsistency(e1, e2)) {
      stop("Images do not occupy the same physical space")
    }
    res <- NA

    if ((e1@pixeltype == "float") |
      (e1@pixeltype == "double") |
      (e2@pixeltype == "float") |
      (e2@pixeltype == "double")) {
      res <- (e1 != 0) | (e2 != 0)
    } else {
      res <- ANTsRCore::antsImageLogicImageImage(e1, e2, "|")
    }
    return(res)
  }
)


############################################
# Arrays
############################################
#' @rdname antsImageops
#' @aliases Logic,antsImage,array-method
setMethod(
  "Logic", signature(e1 = "antsImage", e2 = "array"),
  function(e1, e2) {
    ## either use drop_img_dim and validObject or take out both
    e2 <- as.antsImage(e2, reference = e1)

    res <- callGeneric(e1, e2)
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases Logic,array,antsImage-method
setMethod(
  "Logic", signature(e1 = "array", e2 = "antsImage"),
  function(e1, e2) {
    res <- callGeneric(e2, e1) # reversed order
    return(res)
  }
)


#' @rdname antsImageops
#' @aliases Logic,antsImage,logical-method
setMethod(
  "&", signature(e1 = "antsImage", e2 = "logical"),
  function(e1, e2) {
    res <- NA
    if ((e1@pixeltype == "float") |
      (e1@pixeltype == "double")) {
      res <- (e1 != 0) & e2
    } else {
      res <- ANTsRCore::antsImageLogicImageNumeric(e1, e2, "&")
    }
    return(res)
  }
)


#' @rdname antsImageops
#' @aliases Logic,antsImage,logical-method
setMethod(
  "|", signature(e1 = "antsImage", e2 = "logical"),
  function(e1, e2) {
    res <- NA
    if ((e1@pixeltype == "float") |
      (e1@pixeltype == "double")) {
      res <- (e1 != 0) | e2
    } else {
      res <- ANTsRCore::antsImageLogicImageNumeric(e1, e2, "|")
    }
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases Logic,logical,antsImage-method
setMethod(
  "Logic", signature(e1 = "logical", e2 = "antsImage"),
  function(e1, e2) {
    res <- callGeneric(e2, e1) # reversed order
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases Logic,antsImage,numeric-method
setMethod(
  "Logic", signature(e1 = "antsImage", e2 = "numeric"),
  function(e1, e2) {
    e2 <- as.logical(e2)
    res <- callGeneric(e1, e2)
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases Logic,numeric,antsImage-method
setMethod(
  "Logic", signature(e1 = "numeric", e2 = "antsImage"),
  function(e1, e2) {
    res <- callGeneric(e2, e1) # reversed order
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases Logic,antsImage,ANY-method
setMethod(
  "Logic", signature(e1 = "antsImage", e2 = "ANY"),
  function(e1, e2) {
    ## This is for unary operators
    a1 <- as.array(e1)

    res <- callGeneric(a1, e2)
    res <- as.antsImage(res, reference = e1)
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases Logic,ANY,antsImage-method
setMethod(
  "Logic", signature(e1 = "ANY", e2 = "antsImage"),
  function(e1, e2) {
    res <- callGeneric(e2, e1) # reversed order
    return(res)
  }
)

############################################
# Lists
############################################
#' @rdname antsImageops
#' @aliases Logic,list,antsImage-method
setMethod(
  "Logic", signature(e1 = "list", e2 = "antsImage"),
  function(e1, e2) {
    ## either use drop_img_dim and validObject or take out both
    # a2 = as.array(e1)
    stop("antsRegions not done yet!")
  }
)

#' @rdname antsImageops
#' @aliases Logic,antsImage,list-method
setMethod(
  "Logic", signature(e1 = "antsImage", e2 = "list"),
  function(e1, e2) {
    ## either use drop_img_dim and validObject or take out both
    # a2 = as.array(e1)
    stop("antsRegions not done yet!")
  }
)
