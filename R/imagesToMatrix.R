imagesToMatrix <- function(imageList, mask) {
  n<-length( imageList )
  if ( n < 1 )
    {
    print(" length of input list must be >= 1 ")
    return(NA)
    }
  if ( class(imageList) != "character" )
    {
    print("Must pass a list of filenames")
    return(NA)
    }
  return( .Call("imagesToMatrix", imageList, mask, n ) )
}
