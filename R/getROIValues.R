getROIValues <- function( valueImage, roiImage, maskImage )
  {
  if ( nargs() == 0 )
    {
    print( args( getROIValues ) ) 
    return(1)
    }
  uvals<-sort( unique( valueImage[ maskImage == 1 ] ) )
  roivals<-rep(NA,length(uvals))
  tempimg <- antsImageClone( roiImage )
  ImageMath(3,tempimg,'m',roiImage,maskImage)
  for ( x in 1:length(uvals) )
    {
    inds<-( tempimg == uvals[x]   )
    if ( sum( inds ) > 0 ) roivals[x]<-mean( valueImage[ inds ] )
    }
  return( list( roiValues = uvals,  roiMeans = roivals ) )
  }


