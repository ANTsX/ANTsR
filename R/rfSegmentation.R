rfSegmentation <- function( labelimg, featureimages ) {
  if (nargs() == 0) {
    print("Usage:  probs<-rfSegmentation( x, x2 ) ")
    return(1)
  }
  mask<-antsImageClone( labelimg )
  mask<-getMask( mask )
  labels<-labelimg[ mask == 1 ]
  fmat<-imageListToMatrix( featureimages , mask )
  mydf<-data.frame( labels = labels , fmat )
  myrf<-randomForest( labels ~ . , data=mydf )
  # FIXME make probabilityimages
  probabilityimages<-0
  myout<-list( segs=predict(myrf), probabilityimages = probabilityimages )
  return( myout )
} 
