rfSegmentation <- function( labelimg, featureimages, ntrees=100, verbose=FALSE ) {
  if (nargs() == 0) {
    print("Usage:  probs<-rfSegmentation( x, x2 ) ")
    return(1)
  }
  mask<-antsImageClone( labelimg )
  mask<-getMask( mask )
  labels<-as.factor( labelimg[ mask == 1 ] )
  fmat<-imageListToMatrix( featureimages , mask )
  mydf<-data.frame( labels = labels , fmat )
  myrf<-randomForest( labels ~ . , data=mydf , ntree = ntrees , type = "classification", importance = TRUE, na.action = na.omit , do.trace=verbose )
  if ( verbose ) print( myrf )
  # FIXME make probabilityimages
  probabilityimages<-0
  myout<-list( segs=predict(myrf), probabilityimages = probabilityimages )
  return( myout )
} 
