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
#' renderImageLabels(labels, smoothsval=0.5, alphasurf=0.3, color=snapColors(100) )
#' }
#' 
#' @export renderImageLabels
renderImageLabels <- function(labelsimg, surfval = 0.5, smoothsval = 0, blobrender = TRUE, 
  alphasurf = 1, alphafunc = 1, outdir = "./", outfn = NA, physical = TRUE, color = c(), 
  labels = FALSE) {
  if (missing(labelsimg)) {
    stop("Check usage:  at minimum, you need to call \n renderSurfaceFunction( an_ants_image ) \n ")
  }
  
  nLabels <- max(as.array(labelsimg))
  
  colors <- color
  if (length(colors) < 1) {
    colors <- snapColors(nLabels)
  }
  mylist <- list()
  
  for (i in 1:nLabels) {
    print(i)
    limg <- antsImageClone(labelsimg)
    ThresholdImage(3, limg, limg, i, i)
    
    if (smoothsval > 0) {
      SmoothImage(3, limg, smoothsval, limg)
    }
    
    print(sum(as.array(limg)))
    
    surf <- as.array(limg)
    brain <- contour3d(surf, level = surfval, alpha = alphasurf, draw = FALSE, 
      smooth = 1, color = colors[i])
    
    print("convert points")
    if (physical == TRUE) {
      brain$v1 <- antsTransformIndexToPhysicalPoint(limg, brain$v1)
      brain$v2 <- antsTransformIndexToPhysicalPoint(limg, brain$v2)
      brain$v3 <- antsTransformIndexToPhysicalPoint(limg, brain$v3)
    }
    mylist[[i]] <- brain
  }
  
  drawScene.rgl(mylist, add = TRUE)
  return(mylist)
} 
