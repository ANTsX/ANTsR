#' Create a warped grid for visualization.
#'
#' Deforming a grid is a helpful way to visualize a deformation field. This
#' function enables a user to define the grid parameters and apply a deformable
#' map to that grid.
#'
#' @param img input image
#' @param gridStep width of grid blocks
#' @param gridWidth width of grid lines
#' @param gridDirections directions in which to draw grid lines, boolean vector
#' @param fixedReferenceImage reference image space
#' @param transform vector of transforms
#' @param foreground intensity value for grid blocks
#' @param background intensity value for grid lines
#' @return image is output
#' @author Avants BB
#' @examples
#'
#' fi <- antsImageRead(getANTsRData("r16"))
#' mi <- antsImageRead(getANTsRData("r64"))
#' mygr <- createWarpedGrid(mi)
#' \dontrun{
#' mytx <- antsRegistration(fixed = fi, moving = mi, typeofTransform = c("SyN"))
#' mywarpedgrid <- createWarpedGrid(mi,
#'   gridDirections = c(F, T),
#'   transform = mytx$fwdtransforms, fixedReferenceImage = fi
#' )
#' plot(fi, mywarpedgrid, alpha = 0.75, color.overlay = "blue")
#' }
#'
#' @export createWarpedGrid
createWarpedGrid <- function(
    img, gridStep = 10, gridWidth = 2,
    gridDirections = c(TRUE, TRUE), fixedReferenceImage = NULL, transform = NA,
    foreground = 1, background = 0) {
  if (length(gridDirections) != img@dimension) {
    gridDirections <- rep(TRUE, img@dimension)
  }
  garr <- as.array(img) * 0 + foreground
  gridw <- gridWidth
  # FIXME - should figure out how to improve the code below
  for (d in 1:img@dimension)
  {
    togrid <- seq(from = 1, to = (dim(garr)[d] - gridWidth), by = gridStep)
    for (i in 1:length(togrid))
    {
      if (d == 1 & img@dimension == 3 & gridDirections[d]) {
        garr[togrid[i]:(togrid[i] + gridw), , ] <- background
      }
      if (d == 2 & img@dimension == 3 & gridDirections[d]) {
        garr[, togrid[i]:(togrid[i] + gridw), ] <- background
      }
      if (d == 3 & img@dimension == 3 & gridDirections[d]) {
        garr[, , togrid[i]:(togrid[i] + gridw)] <- background
      }
      if (d == 1 & img@dimension == 2 & gridDirections[d]) {
        garr[togrid[i]:(togrid[i] + gridw), ] <- background
      }
      if (d == 2 & img@dimension == 2 & gridDirections[d]) {
        garr[, togrid[i]:(togrid[i] + gridw)] <- background
      }
    }
  }
  gimg <- as.antsImage(garr)
  antsCopyImageInfo(img, gimg)
  if (!any(is.na(transform)) & !is.null(fixedReferenceImage)) {
    fixedReferenceImage <- check_ants(fixedReferenceImage)
    return(antsApplyTransforms(
      fixed = fixedReferenceImage, moving = gimg,
      transformlist = transform
    ))
  } else {
    return(gimg)
  }
}
