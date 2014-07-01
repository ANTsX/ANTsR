projectImageAlongAxis <- function( imageND, referenceImageNDminus1, projtype=0,  axis=NA)
{
  if (nargs() == 0) {
    print("Usage:  x_projected<-projectImageAlongAxis.Rd( x, xdownreference ) ")
    return(1)
  }
  if ( is.na(axis) ) axis<-( imageND@dimension-1 )
  if ( axis >= imageND@dimension ) axis<-( imageND@dimension-1 )
  downimg<-antsImageClone( referenceImageNDminus1 )
  ImageMath( imageND@dimension, downimg, "Project",imageND,projtype,axis)
  antsCopyImageInfo(referenceImageNDminus1,downimg)
  return( downimg )
} 
