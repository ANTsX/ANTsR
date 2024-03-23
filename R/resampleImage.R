#' resampleImage
#'
#' Resample image by spacing or number of voxels with various interpolators.
#' Works with multi-channel images.
#'
#' @param image input antsImage
#' @param resampleParams vector of size dimension with numeric values
#' @param useVoxels true means interpret resample params as voxel counts
#' @param interpType one of 0 (linear), 1 (nearestNeighbor),
#'   2 (gaussian), 3 (windowedSinc), 4 (bspline).  The user can pass either
#'   a numeric or character argument here.
#' @return output antsImage
#' @author Avants BB
#' @examples
#' fi <- antsImageRead(getANTsRData("r16"))
#' r <- range(fi)
#' finn <- resampleImage(fi, c(50, 60), TRUE, 0)
#' range(finn)
#' stopifnot(diff(range(finn)) > 0)
#' filin <- resampleImage(fi, c(1.5, 1.5), FALSE, 1)
#' range(filin)
#' stopifnot(diff(range(filin)) > 0)
#' pixeltype(fi) <- "float"
#' arr <- as.array(fi)
#' arr <- floor(arr)
#' class(arr) <- "integer"
#' fi <- as.antsImage(arr, pixeltype = "unsigned int")
#' filin <- resampleImage(fi, c(1.5, 1.5), FALSE, 1)
#' fi <- as.antsImage(arr > 14, pixeltype = "unsigned char")
#' filin <- resampleImage(fi, c(1.5, 1.5), FALSE, 1)
#'
#' fi <- antsImageRead(getANTsRData("multi_component_image"))
#' filin <- resampleImage(fi, c(2, 2), FALSE, 1)
#'
#' @export resampleImage
resampleImage <- function(image, resampleParams, useVoxels = FALSE, interpType = "nearestneighbor") {
  image <- check_ants(image)
  pixtype <- image@pixeltype

  if (class(interpType) == "character") {
    interpType <- tolower(interpType)
  } else if (class(interpType) == "numeric") {
    if (interpType == 0) {
      interpType <- "linear"
    } else if (interpType == 1) {
      interpType <- "nearestneighbor"
    } else if (interpType == 2) {
      interpType <- "gaussian"
    } else if (interpType == 3) {
      interpType <- "hammingwindowedsinc"
    } else if (interpType == 4) interpType <- "bspline"
  }

  # identify transform
  tx <- createAntsrTransform(dimension = image@dimension)

  iSpacing <- antsGetSpacing(image)
  iOrigin <- antsGetOrigin(image)
  iDirection <- antsGetDirection(image)
  iSize <- dim(image)

  if (useVoxels) {
    oSize <- resampleParams
    ratio <- iSize / oSize
    oSpacing <- iSpacing * ratio
  } else {
    oSpacing <- resampleParams
    oSize <- as.integer((iSpacing * iSize) / oSpacing + 0.5)
  }

  # reference image
  ref <- makeImage(oSize, spacing = oSpacing, direction = iDirection, origin = iOrigin, pixeltype = "unsigned char")
  outimg <- applyAntsrTransformToImage(tx, image, ref, interpolation = interpType)
  return(outimg)
}
