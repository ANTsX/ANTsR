antsImageClone <- function(in_image, out_pixeltype = in_image@pixeltype) {
  .Call("antsImageClone", in_image, out_pixeltype, PACKAGE = "libRantsImageClone")
} 
