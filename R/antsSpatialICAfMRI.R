#' Perform spatial ICA on fMRI bold data.
#'
#' Perform spatial ICA on group or individual fMRI data.  Preprocessing should
#' be performed prior to calling this function (cf preprocessfMRI.R).
#'
#'
#' @param boldImages a list of 4-D ANTs image fMRI data.
#' @param maskImage A 3-D ANTs image defining the region of interest.  This
#' must be specified.
#' @param numberOfICAComponents Number of estimated observers (components).
#' @param normalizeComponentImages Boolean to specify whether each component
#' vector element is normalized to its z-score.
#' @param verbose boolean setting verbosity level.
#' @return Output list includes standard ICA matrices from the fastICA
#' algorithm:
#'
#' X = pre-processed data matrix
#'
#' K = pre-whitening matrix that projects data onto the first n.comp principal
#' components
#'
#' W = estimated un-mixing matrix (see definition in details)
#'
#' A = estimated mixing matrix
#'
#' S = estimated source matrix
#'
#' and the component images.
#' @author Tustison NJ, Avants BB
#' @examples
#'
#' set.seed(2017)
#' boldImages <- list()
#' n <- 16
#' nvox <- n * n * n * 12
#' dims <- c(n, n, n, 12)
#' boldImages[[1]] <- makeImage(dims, rnorm(nvox) + 500)
#' boldImages[[2]] <- makeImage(dims, rnorm(nvox) + 500)
#' boldImages[[3]] <- makeImage(dims, rnorm(nvox) + 500)
#' maskImage <- getAverageOfTimeSeries(boldImages[[1]]) * 0 + 1
#' icaResults <- antsSpatialICAfMRI(boldImages, maskImage,
#'   numberOfICAComponents = 2
#' )
#'
#' @export antsSpatialICAfMRI
antsSpatialICAfMRI <- function(
    boldImages, maskImage = NULL,
    numberOfICAComponents = 20,
    normalizeComponentImages = TRUE, verbose = FALSE) {
  if (is.null(maskImage)) {
    stop("No mask image specified. \n\n")
  }
  if (!usePkg("fastICA")) {
    print("Need fastICA package")
    return(NULL)
  }
  numberOfBoldImages <- length(boldImages)

  # Group ICA is performed by concatenating the time series of the bold images
  # (similar to MELODIC---http://fsl.fmrib.ox.ac.uk/fsl/fslwiki/MELODIC).  Other
  # possible group approaches are described in
  # http://www.ncbi.nlm.nih.gov/pubmed/19059344

  for (i in 1:numberOfBoldImages) {
    # if there's only 1 bold image, then it will be whitened in the fastICA function
    # call

    subjectBoldMatrix <- timeseries2matrix(boldImages[[i]], maskImage)
    if (numberOfBoldImages > 1) {
      subjectBoldMatrix <- whiten(subjectBoldMatrix)
    }

    if (i == 1) {
      groupBoldMatrix <- subjectBoldMatrix
    } else {
      groupBoldMatrix <- rbind(groupBoldMatrix, subjectBoldMatrix)
    }
  }
  #  return( groupBoldMatrix )
  # taken from the fastICA package

  icaResults <- fastICA::fastICA(
    X = t(groupBoldMatrix), n.comp = numberOfICAComponents,
    alg.typ = c("parallel"), fun = c("logcosh"), alpha = 1, method = c("C"),
    row.norm = FALSE, maxit = 200, tol = 1e-04, verbose = verbose, w.init = NULL
  )

  # create componentImages

  componentImages <- list()
  for (i in 1:numberOfICAComponents) {
    componentVector <- icaResults$S[, i]
    if (normalizeComponentImages) {
      componentVector <- scale(componentVector)
    }
    componentImages[[i]] <- antsImageClone(maskImage, "float")
    componentImages[[i]][maskImage != 0] <- componentVector
  }

  # standard ICA output items from the fastICA algorithm X = pre-processed data
  # matrix K = pre-whitening matrix that projects data onto the first n.comp
  # principal components.  W = estimated un-mixing matrix (see definition in
  # details) A = estimated mixing matrix S = estimated source matrix

  # PCA components (whitened matrix) = X %*% K ICA components = S = X %*% K %*% W
  # (the ICA algorithm attempts to find the unmixing matrix, W)

  return(list(
    X = icaResults$X, K = icaResults$K, W = icaResults$W, A = icaResults$A,
    S = icaResults$S, componentImages = componentImages
  ))
}
