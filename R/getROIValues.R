getROIValues <- function( valueImage, roiImage, maskImage )
  {
  if ( nargs() == 0 )
    {
    print( args( getROIValues ) ) 
    return(1)
    }
  uvals<-sort( unique( roiImage[ maskImage == 1 ] ) )
  roivals<-rep(NA,length(uvals))
  tempimg <- antsImageClone( roiImage )
  ImageMath(3,tempimg,'m',roiImage,maskImage)
  print(length(uvals))
  pb <- txtProgressBar( min = 0, max = length(uvals), style = 3 )
  for ( x in 1:length(uvals) )
    {
    setTxtProgressBar( pb , x )
    inds<-( tempimg == uvals[x]   )
    if ( sum( inds ) > 0 ) roivals[x]<-mean( valueImage[ inds ] )
    }
  return( list( roiValues = uvals,  roiMeans = roivals ) )
  }


