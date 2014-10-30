# labels2matrix.R convert a labeled image to an n x m binary matrix where n =
# number of voxels m = number of labels only include values inside the provided
# mask while including background ( img == 0 ) for consistency with
# timeseries2matrix

labels2vector <- function(img, mask) {
  if (length(img) != length(mask)) {
    stop("image and mask must be same size")
  }
  
  vec <- subset(img, mask == 1)
  
  return(vec)
} 
