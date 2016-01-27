.antsMotionCorrStats <- function(inimg, mask, mocoparams, stupidoff=2 ) {
  tsimg <- antsImageClone(inimg, "float")
  mocomat <- as.matrix(mocoparams)
  .Call("antsMotionCorrStats", tsimg, mask, mocomat, stupidoff,
    PACKAGE = "ANTsR")
}

.antsMotionCorrStats0 <- function(inimg, mask, mocoparams, stupidoff=0 ) {
  tsimg <- antsImageClone(inimg, "float")
  mocomat <- as.matrix(mocoparams)
  .Call("antsMotionCorrStats", tsimg, mask, mocomat, stupidoff,
    PACKAGE = "ANTsR")
}
