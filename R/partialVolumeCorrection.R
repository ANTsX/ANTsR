partialVolumeCorrection <- function(img, img.gm, img.wm, mask = NULL, proportion = 0.4) {
  if (class(img)[1] == "antsImage") {
    if (is.null(mask)) {
      mask <- antsImageClone(img)
      mask[img != 0] <- 1
    }
    values.img <- img[mask > 0]
    values.gm <- img.gm[mask > 0]
    values.wm <- img.wm[mask > 0]
  } else if (is.numeric(img)) {
    values.img <- img
    values.gm <- img.gm
    values.wm <- img.wm
  } else stop("Input image must be either antsImage or numeric.")
  
  values.corrected <- values.img/(values.gm + 0.4 * values.wm)
  values.corrected[(values.gm + values.wm) < 0.25] <- values.img[(values.gm + values.wm) < 
    0.25]  # numerical stability
  if (is.numeric(img)) {
    return(values.corrected)
  } else {
    img.corrected <- antsImageClone(img)
    img.corrected[mask > 0] <- values.corrected
    img.corrected[mask == 0] <- 0
    return(img.corrected)
  }
} 
