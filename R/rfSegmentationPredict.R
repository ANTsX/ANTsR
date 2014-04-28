rfSegmentationPredict <- function( rfSegmentationModel, featureimages, mask, verbose=FALSE ) {
  if (nargs() == 0) {
    print("Usage:  probs<-rfSegmentationPredict( rfSegmentationModel, featureimages , mask ) ")
    return(1)
  }
  fmat<-t( imageListToMatrix( featureimages , mask ) )
  mydf<-data.frame( fmat )
  segs<-antsImageClone( mask )
  segs[ mask == 1 ]<-predict( rfSegmentationModel, newdata = mydf )
  return( segs )
} 
