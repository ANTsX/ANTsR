rfSegmentation <- function( labelimg, featureimages, ntrees=100, verbose=FALSE ) {
  if (nargs() == 0) {
    print("Usage:  probs<-rfSegmentation( x, x2 ) ")
    return(1)
  }
  mask<-antsImageClone( labelimg )
  mask<-getMask( mask )
  labels<-as.factor( labelimg[ mask == 1 ] )
  fmat<-t( imageListToMatrix( featureimages , mask ) )
  mydf<-data.frame( labels = labels , fmat )
  myrf<-randomForest( labels ~ . , data=mydf , ntree = ntrees , type = "classification", importance = TRUE, na.action = na.omit , do.trace=verbose )
  if ( verbose ) print( myrf )
  probabilityimages<-predict(myrf, type = "prob" )
  probabilityimages<-matrixToImages( t( probabilityimages ), mask )
  segs<-antsImageClone( mask )
  segs[ mask == 1 ]<-predict( myrf )
  myout<-list( segmentation=segs, probabilityimages = probabilityimages,  rfModel=myrf )
  return( myout )
} 
