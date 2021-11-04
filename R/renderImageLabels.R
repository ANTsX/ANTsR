#' 3D surface-based rendering of image segmentation labels
#'
#' Will use rgl to render surfaces
#'
#'
#' @param labelsimg 3D images of integer labels
#' @param surfval intensity level that defines isosurface
#' @param smoothsval sigma for smoothing of each extracted label image
#' @param alphasurf opacity of each rendered surface
#' @param physical flag to use true spatial coordinates
#' @param color colors to use for each label
#' @return 0 -- Success\cr 1 -- Failure
#' @author Duda, J
#' @examples
#'
#' \dontrun{
#' renderImageLabels(labels)
#' renderImageLabels(labels, smoothsval=0.5, alphasurf=0.3  )
#' }
#'
#' @export renderImageLabels
renderImageLabels <- function(
  labelsimg,
  surfval = 0.5,
  smoothsval = 0,
  alphasurf = 1,
  physical = TRUE,
  color = c() ) {
  if (missing(labelsimg)) {
    stop("Check usage:  at minimum, you need to call \n renderSurfaceFunction( an_ants_image ) \n ")
  }
  if(!usePkg("rgl") || !usePkg('misc3d')) {
    print("rgl and misc3d are necessary for this function.")
    return(NULL)
  }
  labelnums = sort(unique(segL))[-1]
  nLabels <- length( labelnums )
  colors <- color
  if (length(colors) < 1) {
    colors <- .snapColors(nLabels)
  }
  mylist <- list()

  for (i in 1:nLabels) {
    labelsimgloc<-thresholdImage( labelsimg, labelnums[i], labelnums[i] )

    if (smoothsval > 0) {
      labelsimg<-smoothImage(labelsimgloc, smoothsval)
    }

    surf <- as.array(labelsimgloc)
    brain <- misc3d::contour3d(surf, level = surfval, alpha = alphasurf, draw = FALSE,
      smooth = 1, color = colors[i])

    if (physical == TRUE) {
      brain$v1 <- antsTransformIndexToPhysicalPoint(labelsimg, brain$v1)
      brain$v2 <- antsTransformIndexToPhysicalPoint(labelsimg, brain$v2)
      brain$v3 <- antsTransformIndexToPhysicalPoint(labelsimg, brain$v3)
    }
    mylist[[i]] <- brain
  }
  misc3d::drawScene.rgl(mylist, add = TRUE)
  return(mylist)
}
