check_numeric_components <- function(image, number) {
  if (length(number) != components(image) &
    components(image) > 1) {
    stop(paste0(
      "Comparison value not the same length as number of channels in image"
    ))
  }
}


check_components <- function(image) {
  if (components(image) > 1) {
    stop(paste0(
      "For multiple component images, comparison",
      " is not implemented yet, use `splitChannels` and lapply, then mergeChannels"
    ))
  }
}

rep_number <- function(image, number) {
  if (length(number) != components(image) &
    components(image) > 1 &
    length(number) == 1) {
    number <- rep(number, components(image))
  }
  return(number)
}

#' @rdname antsImageops
#' @aliases ==,antsImage,antsImage-method
#' @examples
#' img01 <- as.antsImage(array(1:64, c(4, 4, 4, 1)))
#' et <- function(x) {
#'   testthat::expect_true(x)
#' }
#' x <- is.antsImage(img01 == img01)
#' et(x)
#' x <- is.antsImage(img01 == as.array(img01))
#' et(x)
#' x <- is.antsImage(img01 == 1)
#' et(x)
#' x <- is.antsImage(1 == img01)
#' et(x)
#' x <- is.antsImage(as.array(img01) == img01)
#' et(x)
#'
#' x <- is.antsImage(img01 >= img01)
#' et(x)
#' x <- is.antsImage(img01 >= as.array(img01))
#' et(x)
#' x <- is.antsImage(img01 >= 1)
#' et(x)
#' x <- is.antsImage(1 >= img01)
#' et(x)
#' x <- is.antsImage(as.array(img01) >= img01)
#' et(x)
#'
#' x <- is.antsImage(img01 > img01)
#' et(x)
#' x <- is.antsImage(img01 > as.array(img01))
#' et(x)
#' x <- is.antsImage(img01 > 1)
#' et(x)
#' x <- is.antsImage(1 > img01)
#' et(x)
#' x <- is.antsImage(as.array(img01) > img01)
#' et(x)
#'
#' x <- is.antsImage(img01 < img01)
#' et(x)
#' x <- is.antsImage(img01 < as.array(img01))
#' et(x)
#' x <- is.antsImage(img01 < 1)
#' et(x)
#' x <- is.antsImage(1 < img01)
#' et(x)
#' x <- is.antsImage(as.array(img01) < img01)
#' et(x)
#'
#'
#' x <- is.antsImage(img01 <= img01)
#' et(x)
#' x <- is.antsImage(img01 <= as.array(img01))
#' et(x)
#' x <- is.antsImage(1 <= img01)
#' et(x)
#' x <- is.antsImage(img01 <= 1)
#' et(x)
#' x <- is.antsImage(as.array(img01) <= img01)
#' et(x)
#'
#' x <- is.antsImage(img01 != img01)
#' et(x)
#' x <- is.antsImage(img01 != as.array(img01))
#' et(x)
#' x <- is.antsImage(1 != img01)
#' et(x)
#' x <- is.antsImage(img01 != 1)
#' et(x)
#' x <- is.antsImage(as.array(img01) != img01)
#' et(x)
#'
#' ###########################
#' # Multi image operations
#' ########################
#' fname <- system.file("extdata",
#'   "multi_component_image.nii.gz",
#'   package = "ANTsRCore"
#' )
#' img <- antsImageRead(fname)
#'
#' testthat::expect_is(img > 0, "antsImage")
#' testthat::expect_is(0 < img, "antsImage")
#' testthat::expect_is(img > c(0, 0), "antsImage")
#'
#' testthat::expect_is(img >= 0, "antsImage")
#' testthat::expect_is(0 <= img, "antsImage")
#' testthat::expect_is(img >= c(0, 0), "antsImage")
#'
#' testthat::expect_is(img < 0, "antsImage")
#' testthat::expect_is(0 > img, "antsImage")
#' testthat::expect_is(img < c(0, 0), "antsImage")
#'
#' testthat::expect_is(img <= 0, "antsImage")
#' testthat::expect_is(0 >= img, "antsImage")
#' testthat::expect_is(img <= c(0, 0), "antsImage")
#'
#' testthat::expect_is(img == 0, "antsImage")
#' testthat::expect_is(0 == img, "antsImage")
#' testthat::expect_is(img == c(0, 0), "antsImage")
#'
#' testthat::expect_is(img != 0, "antsImage")
#' testthat::expect_is(0 != img, "antsImage")
#' testthat::expect_is(img != c(0, 0), "antsImage")
#'
setMethod(
  "==", signature(e1 = "antsImage", e2 = "antsImage"),
  function(e1, e2) {
    operator <- "=="
    ## either use drop_img_dim and validObject or take out both
    if (!antsImagePhysicalSpaceConsistency(e1, e2)) {
      stop("Images do not occupy the same physical space")
    }
    res <- ANTsRCore::antsImageComparisonImageImage(e1, e2, operator)
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases ==,antsImage,list-method
#' @examples
#' img01 <- as.antsImage(array(1:64, c(4, 4, 4, 1)))
#' testthat::expect_error(img01 == list())
setMethod(
  "==", signature(e1 = "antsImage", e2 = "list"),
  function(e1, e2) {
    stop("antsRegions not done yet!")
  }
)

#' @rdname antsImageops
#' @aliases ==,antsImage,array-method
setMethod(
  "==", signature(e1 = "antsImage", e2 = "array"),
  function(e1, e2) {
    e2 <- as.antsImage(e2, reference = e1)
    res <- e1 == e2
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases ==,array,antsImage-method
setMethod(
  "==", signature(e1 = "array", e2 = "antsImage"),
  function(e1, e2) {
    i1 <- as.antsImage(e1, reference = e2)
    return(i1 == e2)
  }
)

#' @rdname antsImageops
#' @aliases ==,antsImage,ANY-method
setMethod(
  "==", signature(e1 = "antsImage", e2 = "ANY"),
  function(e1, e2) {
    operator <- "=="
    if (components(e1) == 1) {
      if (length(e2) == 1) {
        e2 <- as.numeric(e2)
        # e2 = rep_number(e2, image = e1)
        # check_numeric_components(image = e1, number = e2)
        res <- ANTsRCore::antsImageComparisonImageNumeric(e1, e2, operator)
        return(res)
      }
    }
    a1 <- as.array(e1)
    res <- callGeneric(a1, e2)
    res <- as.antsImage(res, reference = e1)
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases ==,ANY,antsImage-method
setMethod(
  "==", signature(e1 = "ANY", e2 = "antsImage"),
  function(e1, e2) {
    # e1 = rep_number(e1, image = e2)
    # check_numeric_components(image = e2, number = e1)
    a2 <- as.array(e2)
    res <- callGeneric(e1, a2)
    res <- as.antsImage(res, reference = e2)
    return(res) # Now returns antsImage
  }
)






#' @rdname antsImageops
#' @aliases >,antsImage,antsImage-method
setMethod(
  ">", signature(e1 = "antsImage", e2 = "antsImage"),
  function(e1, e2) {
    operator <- ">"
    ## either use drop_img_dim and validObject or take out both
    if (!antsImagePhysicalSpaceConsistency(e1, e2)) {
      stop("Images do not occupy the same physical space")
    }
    res <- ANTsRCore::antsImageComparisonImageImage(e1, e2, operator)
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases >,antsImage,list-method
#' @examples
#' img01 <- as.antsImage(array(1:64, c(4, 4, 4, 1)))
#' testthat::expect_error(img01 > list())
setMethod(
  ">", signature(e1 = "antsImage", e2 = "list"),
  function(e1, e2) {
    stop("antsRegions not done yet!")
  }
)

#' @rdname antsImageops
#' @aliases >,antsImage,array-method
setMethod(
  ">", signature(e1 = "antsImage", e2 = "array"),
  function(e1, e2) {
    e2 <- as.antsImage(e2, reference = e1)
    res <- e1 > e2
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases >,array,antsImage-method
setMethod(
  ">", signature(e1 = "array", e2 = "antsImage"),
  function(e1, e2) {
    i1 <- as.antsImage(e1, reference = e2)
    return(i1 > e2)
  }
)

#' @rdname antsImageops
#' @aliases >,antsImage,ANY-method
setMethod(
  ">", signature(e1 = "antsImage", e2 = "ANY"),
  function(e1, e2) {
    operator <- ">"
    if (components(e1) == 1) {
      if (length(e2) == 1) {
        e2 <- as.numeric(e2)
        # e2 = rep_number(e2, image = e1)
        # check_numeric_components(image = e1, number = e2)
        res <- ANTsRCore::antsImageComparisonImageNumeric(e1, e2, operator)
        return(res)
      }
    }
    a1 <- as.array(e1)
    res <- callGeneric(a1, e2)
    res <- as.antsImage(res, reference = e1)
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases >,ANY,antsImage-method
setMethod(
  ">", signature(e1 = "ANY", e2 = "antsImage"),
  function(e1, e2) {
    # check_numeric_components(image = e2, number = e1)
    # e1 = rep_number(e1, image = e2)
    a2 <- as.array(e2)
    res <- callGeneric(e1, a2)
    res <- as.antsImage(res, reference = e2)
    return(res) # Now returns antsImage
  }
)






#' @rdname antsImageops
#' @aliases <,antsImage,antsImage-method
setMethod(
  "<", signature(e1 = "antsImage", e2 = "antsImage"),
  function(e1, e2) {
    operator <- "<"
    ## either use drop_img_dim and validObject or take out both
    if (!antsImagePhysicalSpaceConsistency(e1, e2)) {
      stop("Images do not occupy the same physical space")
    }
    res <- ANTsRCore::antsImageComparisonImageImage(e1, e2, operator)
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases <,antsImage,list-method
#' @examples
#' img01 <- as.antsImage(array(1:64, c(4, 4, 4, 1)))
#' testthat::expect_error(img01 < list())
setMethod(
  "<", signature(e1 = "antsImage", e2 = "list"),
  function(e1, e2) {
    stop("antsRegions not done yet!")
  }
)

#' @rdname antsImageops
#' @aliases <,antsImage,array-method
setMethod(
  "<", signature(e1 = "antsImage", e2 = "array"),
  function(e1, e2) {
    e2 <- as.antsImage(e2, reference = e1)
    res <- e1 < e2
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases <,array,antsImage-method
setMethod(
  "<", signature(e1 = "array", e2 = "antsImage"),
  function(e1, e2) {
    i1 <- as.antsImage(e1, reference = e2)
    return(i1 < e2)
  }
)

#' @rdname antsImageops
#' @aliases <,antsImage,ANY-method
setMethod(
  "<", signature(e1 = "antsImage", e2 = "ANY"),
  function(e1, e2) {
    operator <- "<"
    if (components(e1) == 1) {
      if (length(e2) == 1) {
        e2 <- as.numeric(e2)
        # e2 = rep_number(e2, image = e1)
        # check_numeric_components(image = e1, number = e2)
        res <- ANTsRCore::antsImageComparisonImageNumeric(e1, e2, operator)
        return(res)
      }
    }
    a1 <- as.array(e1)
    res <- callGeneric(a1, e2)
    res <- as.antsImage(res, reference = e1)
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases <,ANY,antsImage-method
setMethod(
  "<", signature(e1 = "ANY", e2 = "antsImage"),
  function(e1, e2) {
    # check_numeric_components(image = e2, number = e1)
    # e1 = rep_number(e1, image = e2)
    a2 <- as.array(e2)
    res <- callGeneric(e1, a2)
    res <- as.antsImage(res, reference = e2)
    return(res) # Now returns antsImage
  }
)






#' @rdname antsImageops
#' @aliases !=,antsImage,antsImage-method
setMethod(
  "!=", signature(e1 = "antsImage", e2 = "antsImage"),
  function(e1, e2) {
    operator <- "!="
    ## either use drop_img_dim and validObject or take out both
    if (!antsImagePhysicalSpaceConsistency(e1, e2)) {
      stop("Images do not occupy the same physical space")
    }
    res <- ANTsRCore::antsImageComparisonImageImage(e1, e2, operator)
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases !=,antsImage,list-method
#' @examples
#' img01 <- as.antsImage(array(1:64, c(4, 4, 4, 1)))
#' testthat::expect_error(img01 != list())
setMethod(
  "!=", signature(e1 = "antsImage", e2 = "list"),
  function(e1, e2) {
    stop("antsRegions not done yet!")
  }
)

#' @rdname antsImageops
#' @aliases !=,antsImage,array-method
setMethod(
  "!=", signature(e1 = "antsImage", e2 = "array"),
  function(e1, e2) {
    e2 <- as.antsImage(e2, reference = e1)
    res <- e1 != e2
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases !=,array,antsImage-method
setMethod(
  "!=", signature(e1 = "array", e2 = "antsImage"),
  function(e1, e2) {
    i1 <- as.antsImage(e1, reference = e2)
    return(i1 != e2)
  }
)

#' @rdname antsImageops
#' @aliases !=,antsImage,ANY-method
setMethod(
  "!=", signature(e1 = "antsImage", e2 = "ANY"),
  function(e1, e2) {
    operator <- "!="
    if (components(e1) == 1) {
      if (length(e2) == 1) {
        e2 <- as.numeric(e2)
        # e2 = rep_number(e2, image = e1)
        # check_numeric_components(image = e1, number = e2)
        res <- ANTsRCore::antsImageComparisonImageNumeric(e1, e2, operator)
        return(res)
      }
    }
    a1 <- as.array(e1)
    res <- callGeneric(a1, e2)
    res <- as.antsImage(res, reference = e1)
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases !=,ANY,antsImage-method
setMethod(
  "!=", signature(e1 = "ANY", e2 = "antsImage"),
  function(e1, e2) {
    # check_numeric_components(image = e2, number = e1)
    # e1 = rep_number(e1, image = e2)
    a2 <- as.array(e2)
    res <- callGeneric(e1, a2)
    res <- as.antsImage(res, reference = e2)
    return(res) # Now returns antsImage
  }
)






#' @rdname antsImageops
#' @aliases <=,antsImage,antsImage-method
setMethod(
  "<=", signature(e1 = "antsImage", e2 = "antsImage"),
  function(e1, e2) {
    operator <- "<="
    ## either use drop_img_dim and validObject or take out both
    if (!antsImagePhysicalSpaceConsistency(e1, e2)) {
      stop("Images do not occupy the same physical space")
    }
    res <- ANTsRCore::antsImageComparisonImageImage(e1, e2, operator)
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases <=,antsImage,list-method
#' @examples
#' img01 <- as.antsImage(array(1:64, c(4, 4, 4, 1)))
#' testthat::expect_error(img01 <= list())
setMethod(
  "<=", signature(e1 = "antsImage", e2 = "list"),
  function(e1, e2) {
    stop("antsRegions not done yet!")
  }
)

#' @rdname antsImageops
#' @aliases <=,antsImage,array-method
setMethod(
  "<=", signature(e1 = "antsImage", e2 = "array"),
  function(e1, e2) {
    e2 <- as.antsImage(e2, reference = e1)
    res <- e1 <= e2
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases <=,array,antsImage-method
setMethod(
  "<=", signature(e1 = "array", e2 = "antsImage"),
  function(e1, e2) {
    i1 <- as.antsImage(e1, reference = e2)
    return(i1 <= e2)
  }
)

#' @rdname antsImageops
#' @aliases <=,antsImage,ANY-method
setMethod(
  "<=", signature(e1 = "antsImage", e2 = "ANY"),
  function(e1, e2) {
    operator <- "<="
    if (components(e1) == 1) {
      if (length(e2) == 1) {
        e2 <- as.numeric(e2)
        # e2 = rep_number(e2, image = e1)
        # check_numeric_components(image = e1, number = e2)
        res <- ANTsRCore::antsImageComparisonImageNumeric(e1, e2, operator)
        return(res)
      }
    }
    a1 <- as.array(e1)
    res <- callGeneric(a1, e2)
    res <- as.antsImage(res, reference = e1)
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases <=,ANY,antsImage-method
setMethod(
  "<=", signature(e1 = "ANY", e2 = "antsImage"),
  function(e1, e2) {
    # check_numeric_components(image = e2, number = e1)
    # e1 = rep_number(e1, image = e2)
    a2 <- as.array(e2)
    res <- callGeneric(e1, a2)
    res <- as.antsImage(res, reference = e2)
    return(res) # Now returns antsImage
  }
)






#' @rdname antsImageops
#' @aliases >=,antsImage,antsImage-method
setMethod(
  ">=", signature(e1 = "antsImage", e2 = "antsImage"),
  function(e1, e2) {
    operator <- ">="
    ## either use drop_img_dim and validObject or take out both
    if (!antsImagePhysicalSpaceConsistency(e1, e2)) {
      stop("Images do not occupy the same physical space")
    }
    res <- ANTsRCore::antsImageComparisonImageImage(e1, e2, operator)
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases >=,antsImage,list-method
#' @examples
#' img01 <- as.antsImage(array(1:64, c(4, 4, 4, 1)))
#' testthat::expect_error(img01 >= list())
setMethod(
  ">=", signature(e1 = "antsImage", e2 = "list"),
  function(e1, e2) {
    stop("antsRegions not done yet!")
  }
)

#' @rdname antsImageops
#' @aliases >=,antsImage,array-method
setMethod(
  ">=", signature(e1 = "antsImage", e2 = "array"),
  function(e1, e2) {
    e2 <- as.antsImage(e2, reference = e1)
    res <- e1 >= e2
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases >=,array,antsImage-method
setMethod(
  ">=", signature(e1 = "array", e2 = "antsImage"),
  function(e1, e2) {
    i1 <- as.antsImage(e1, reference = e2)
    return(i1 >= e2)
  }
)

#' @rdname antsImageops
#' @aliases >=,antsImage,ANY-method
setMethod(
  ">=", signature(e1 = "antsImage", e2 = "ANY"),
  function(e1, e2) {
    operator <- ">="
    if (components(e1) == 1) {
      if (length(e2) == 1) {
        e2 <- as.numeric(e2)
        # e2 = rep_number(e2, image = e1)
        # check_numeric_components(image = e1, number = e2)
        res <- ANTsRCore::antsImageComparisonImageNumeric(e1, e2, operator)
        return(res)
      }
    }
    a1 <- as.array(e1)
    res <- callGeneric(a1, e2)
    res <- as.antsImage(res, reference = e1)
    return(res)
  }
)

#' @rdname antsImageops
#' @aliases >=,ANY,antsImage-method
setMethod(
  ">=", signature(e1 = "ANY", e2 = "antsImage"),
  function(e1, e2) {
    # check_numeric_components(image = e2, number = e1)
    # e1 = rep_number(e1, image = e2)
    a2 <- as.array(e2)
    res <- callGeneric(e1, a2)
    res <- as.antsImage(res, reference = e2)
    return(res) # Now returns antsImage
  }
)
