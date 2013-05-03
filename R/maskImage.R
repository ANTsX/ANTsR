maskImage <- function(img.in,img.mask, level=1) {
  img.out <- antsImageClone(img.in)
  img.out[ img.mask != level ] <- 0
  return(img.out)
}
