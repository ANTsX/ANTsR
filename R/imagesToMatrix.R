imagesToMatrix <- function(imageList, mask) {
  n<-length( imageList )
  return( .Call("imagesToMatrix", imageList, mask, n ) )
}
