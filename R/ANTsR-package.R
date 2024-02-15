#' @title Advanced Normalization Tools in R
#' @name ANTsR
#'
#' @importFrom magrittr %>%
#' @importFrom ANTsRCore check_ants
#'   antsRegistration atropos thresholdImage labelClusters
#'   antsApplyTransforms antsApplyTransformsToPoints
#'   antsCopyImageInfo antsGetDirection antsGetOrigin antsGetSpacing
#'   antsImageClone antsImageIterator antsImageIteratorGet
#'   antsImageIteratorGetIndex antsImageIteratorIsAtEnd
#'   antsImageIteratorNext antsImageIteratorSet antsImageRead
#'   antsImageWrite antsRegistration antsSetDirection antsSetOrigin
#'   antsSetSpacing antsTransformIndexToPhysicalPoint
#'   antsTransformPhysicalPointToIndex antsrGetPointerName
#'   antsrTransformFromDisplacementField applyAntsrTransform
#'   applyAntsrTransformToImage as.antsImage atropos jointLabelFusion
#'   bigLMStats composeAntsrTransforms imageListToMatrix matrixToImages
#'   cropImage cropIndices getANTsRData getAntsrTransformParameters getMask
#'   getNeighborhoodAtVoxel getNeighborhoodInMask
#'   iMath is.antsImage kmeansSegmentation labelClusters randomMask
#'   labelStats lappend makeImage mergeChannels n3BiasFieldCorrection
#'   readAntsrTransform resampleImage resampleImageToTarget smoothImage
#'   splitChannels thresholdImage usePkg
#' @importFrom graphics hist par plot points
#' @importFrom grDevices colorRampPalette dev.off hsv png rainbow rgb
#' @importFrom methods new
#' @importFrom stats ar as.formula coefficients convolve
#'   cor cor.test cov dist formula glm lm
#'   lm.fit loess median model.matrix na.omit
#'   optimize p.adjust pchisq pf pnorm ppois
#'   predict pt qchisq qf qnorm qt quantile
#'   residuals rnorm sd spec.pgram spline stl
#'   t.test toeplitz ts var
#' @importFrom utils data glob2rx read.csv setTxtProgressBar tail
#'   txtProgressBar write.csv
#'
#' @keywords internal
"_PACKAGE"
#' @importFrom rlang .data
# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
