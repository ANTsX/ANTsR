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
#' set.seed( 123 )
#' boldImages <- list()
#' nvox <- 8*8*8*10
#' dims <- c(8,8,8,10)
#' boldImages[[1]] <- makeImage( dims , rnorm( nvox )+500 ) %>% iMath("PadImage" , 4 )
#' boldImages[[2]] <- makeImage( dims , rnorm( nvox )+500 ) %>% iMath("PadImage" , 4 )
#' boldImages[[3]] <- makeImage( dims , rnorm( nvox )+500 ) %>% iMath("PadImage" , 4 )
#' boldImages[[4]] <- makeImage( dims , rnorm( nvox )+500 ) %>% iMath("PadImage" , 4 )
#'
#' cleanBoldImages <- list()
#' for( i in 1:length( boldImages ) )
#'   {
#'   fmri <- preprocessfMRI( boldImages[[i]] )
#'   if( i == 1 )
#'     {
#'     maskImage <- fmri$maskImage
#'     }
#'   cleanBoldImages[[i]] <- fmri$cleanBoldImage
#'   }
#'
#' icaResults <- antsSpatialICAfMRI( cleanBoldImages, maskImage,
#'   numberOfICAComponents = 2 )
#' componentImages <- icaResults$componentImages
#'
#' # write out the component images
#' for( i in 1:length( icaResults$componentImages ) )
#'   {
#'   componentFileName <- paste0( 'componentImage', i, '.nii.gz' )
#'   cat( 'Writing ', componentFileName, '.\n' )
#'   # antsImageWrite( componentImages[[i]], componentFileName )
#'  }
#'
#' @export antsSpatialICAfMRI
antsSpatialICAfMRI <- function(boldImages, maskImage = NA, numberOfICAComponents = 20,
  normalizeComponentImages = TRUE) {

  if (is.na(maskImage)) {
    stop("No mask image specified. \n\n")
  }
  if ( !usePkg("fastICA") ) { print("Need fastICA package"); return(NULL) }
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

  # taken from the fastICA package

  icaResults <- fastICA::fastICA(X = t(groupBoldMatrix), n.comp = numberOfICAComponents,
    alg.typ = c("parallel"), fun = c("logcosh"), alpha = 1, method = c("C"),
    row.norm = FALSE, maxit = 200, tol = 1e-04, verbose = TRUE, w.init = NULL)

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

  return(list(X = icaResults$X, K = icaResults$K, W = icaResults$W, A = icaResults$A,
    S = icaResults$S, componentImages = componentImages))
}
