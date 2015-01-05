ResampleImage <- function(...) {
  veccer<-c(...)
  veccer2<-veccer
  img1<-antsImageClone(veccer[[2]])
  img1d<-antsImageClone(img1,"double")
  img2<-antsImageClone(veccer[[3]])
  img2d<-antsImageClone(img2,"double")
  veccer2[[2]]<-img1d
  veccer2[[3]]<-img2d
  .Call("ResampleImage", int_antsProcessArguments(veccer2) )
  outimg<-antsImageClone( veccer2[[3]],img1@pixeltype)
  return( outimg )
}
