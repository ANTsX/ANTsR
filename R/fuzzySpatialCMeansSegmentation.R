#' fuzzySpatialCMeansSegmentation
#'
#' Fuzzy spatial c-means for image segmentation.
#'
#' Image segmentation using fuzzy spatial c-means as described in
#'
#' Chuang et al., Fuzzy c-means clustering with spatial information for image
#' segmentation.  CMIG: 30:9-15, 2006.
#'
#' @param image image to be segmented.
#' @param mask optional mask image.  Otherwise, the entire image is used.
#' @param numberOfClusters number of segmentation clusters
#' @param m fuzziness parameter (default = 2).
#' @param p membership importance parameter (default = 1).
#' @param q spatial constraint importance parameter (default = 1).
#' \code{q = 0} is equivalent to conventional fuzzy c-means.
#' @param radius neighborhood radius (scalar or array) for spatial
#' constraint.
#' @param maxNumberOfIterations iteration limit (default = 20).
#' @param convergenceThreshold Convergence between iterations is measured
#' using the Dice coefficient (default = 0.02).
#' @param verbose print progress.
#' @return list containing segmentation and probability images
#'
#' @author NJ Tustison
#'
#' @examples
#' image <- antsImageRead(getANTsRData("r16"))
#' mask <- getMask(image)
#' fuzzy <- fuzzySpatialCMeansSegmentation(image, mask)
#'
#' @export fuzzySpatialCMeansSegmentation

fuzzySpatialCMeansSegmentation <- function(
    image, mask = NULL, numberOfClusters = 4,
    m = 2, p = 1, q = 1, radius = 2, maxNumberOfIterations = 20, convergenceThreshold = 0.02,
    verbose = FALSE) {
  if (is.null(mask)) {
    mask <- antsImageClone(image) * 0 + 1
  }

  x <- image[mask != 0]

  v <- seq(from = 0, to = 1, length.out = numberOfClusters + 2)[2:(numberOfClusters + 1)]
  v <- v * (max(x) - min(x)) + min(x)
  cc <- length(v)

  if (verbose == TRUE) {
    cat("Initial cluster centers: ", v, "\n")
  }

  xx <- matrix(data = 0, nrow = cc, ncol = length(x))
  for (i in seq.int(cc))
  {
    xx[i, ] <- x
  }

  if (length(radius) == 1) {
    radius <- rep(radius, image@dimension)
  }

  segmentation <- antsImageClone(image) * 0
  probabilityImages <- NULL

  iter <- 0
  diceValue <- 0
  while (iter < maxNumberOfIterations && diceValue < 1.0 - convergenceThreshold) {
    # Update membership values

    xv <- matrix(data = 0, nrow = cc, ncol = length(x))
    for (k in seq.int(cc))
    {
      xv[k, ] <- abs(x - v[k])
    }

    u <- matrix(data = 0, nrow = nrow(xv), ncol = ncol(xv))
    for (i in seq.int(cc))
    {
      n <- xv[i, ]

      d <- n * 0
      for (k in seq.int(cc))
      {
        d <- d + (n / xv[k, ])^(2 / (m - 1))
      }
      u[i, ] <- 1 / d
    }
    u[is.nan(u)] <- 1


    # Update cluster centers

    v <- rowSums((u^m) * xx, na.rm = TRUE) / rowSums(u^m, na.rm = TRUE)

    if (verbose == TRUE) {
      cat("Updated cluster centers: ", v, "\n")
    }

    # Spatial function

    h <- matrix(data = 0, nrow = nrow(u), ncol = ncol(u))
    for (i in seq.int(cc))
    {
      uImage <- antsImageClone(image) * 0
      uImage[mask != 0] <- u[i, ]
      uNeighborhoods <- getNeighborhoodInMask(uImage, mask, radius)
      h[i, ] <- colSums(uNeighborhoods, na.rm = TRUE)
    }

    # u prime

    d <- rep(0, ncol(u))
    for (k in seq.int(cc))
    {
      d <- d + (u[k, ]^p) * (h[k, ]^q)
    }

    probabilityImages <- list()
    uprime <- matrix(data = 0, nrow = nrow(u), ncol = ncol(u))
    for (i in seq.int(cc))
    {
      uprime[i, ] <- (u[i, ]^p) * (h[i, ]^q) / d
      uprimeImage <- antsImageClone(image) * 0
      uprimeImage[mask != 0] <- uprime[i, ]
      probabilityImages[[i]] <- uprimeImage
    }

    tmpSegmentation <- antsImageClone(image) * 0
    tmpSegmentation[mask != 0] <- max.col(t(uprime))

    diceValue <- labelOverlapMeasures(segmentation, tmpSegmentation)$MeanOverlap[1]
    iter <- iter + 1

    if (verbose == TRUE) {
      cat("Iteration ", iter, " (out of ", maxNumberOfIterations, "):  ",
        "Dice overlap = ", diceValue, "\n",
        sep = ""
      )
    }
    segmentation <- tmpSegmentation
  }
  return(list(
    segmentationImage = segmentation,
    probabilityImages = probabilityImages
  ))
}
