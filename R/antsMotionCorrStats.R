.antsMotionCorrStats <- function(inimg, mask, mocoparams) {
  tsimg <- antsImageClone(inimg, "float")
  mocomat <- as.matrix(mocoparams)
  .Call("antsMotionCorrStats", tsimg, mask, mocomat,
    PACKAGE = "ANTsR")
}
