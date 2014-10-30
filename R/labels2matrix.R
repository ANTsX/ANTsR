# labels2matrix.R convert a labeled image to an n x m binary matrix where n =
# number of voxels m = number of labels only include values inside the provided
# mask while including background ( img == 0 ) for consistency with
# timeseries2matrix

labels2matrix <- function(img, mask) {
  if (length(img) != length(mask)) {
    stop("image and mask must be same size")
  }
  
  vec <- subset(img, mask > 0)
  nLabels <- max(vec)
  labels <- matrix(0, nrow = length(vec), ncol = nLabels)
  
  for (i in 1:nLabels) {
    labels[, i] <- (vec == i)
  }
  
  return(labels)
} 
