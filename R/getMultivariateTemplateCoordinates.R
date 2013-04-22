getMultivariateTemplateCoordinates <- function( imageSetToBeLabeledIn, templateWithLabels , labelnames = NA , outprefix = NA , convertToTal = FALSE )
  ########################################################################################################
  # this function is similar to getTemplateCoordinates
  # however we need to get the coordinates for each of the entries in imageSetToBeLabeled
  # where coordinates come from each subcomponent of the image in imageSetToBeLabeled
  # pseudo-code:
  #  1. transform template to talairach
  #  2. label the multivariate component image by its generic Talairach position i.e. L/R, S/I, A/P
  #     --- information needed for this is given by getTemplateCoordinates
  #  3. split each image into its sub components via image2ClusterImages
  #  4. get coordinates for each sub-component image
  #  5. append these coordinates under the output from step 2
  #  6. return something e.g. a table ....
  ########################################################################################################
  {
  myout<-NA
  talregions<-rep(NA,length(imageSetToBeLabeledIn)-1)
  for ( x in 2:length(imageSetToBeLabeledIn) )
    {
    img<-imageSetToBeLabeledIn[[x]]
    imageSetToBeLabeled<-list( img )
    threshimg<-antsImageClone( img )
    thresh<-1.0 / length( as.array(threshimg) )
    ImageMath( threshimg@dimension, threshimg,"abs",threshimg )
    threshimg[ threshimg > (.Machine$double.eps*2) ]<-1
    imageSetToBeLabeled2<-lappend( imageSetToBeLabeled , threshimg  ) 
    temp<-getTemplateCoordinates(  imageSetToBeLabeled2, templateWithLabels , labelnames  , outprefix, convertToTal )
    talRegion<-""
    if ( temp$templatepoints$x < 0 ) talRegion<-paste(talRegion,"L",sep='') else talRegion<-paste(talRegion,"R",sep='')
    if ( temp$templatepoints$y > 0 ) talRegion<-paste(talRegion,"A",sep='') else talRegion<-paste(talRegion,"P",sep='')
    if ( temp$templatepoints$z < 0 ) talRegion<-paste(talRegion,"S",sep='') else talRegion<-paste(talRegion,"I",sep='')
    talregions[x-1]<-talRegion
    clust<-image2ClusterImages( img )
    clust<-eigSeg( threshimg, clust )
    imageSetToBeLabeled2<-lappend( imageSetToBeLabeled , clust  ) 
    temp2<-getTemplateCoordinates(  imageSetToBeLabeled2, templateWithLabels , labelnames  , outprefix, convertToTal )
    if ( x == 2 )
      {
      myout<-data.frame( NetworkID="N1_omnibus",temp$templatepoints)
      subnet<-data.frame( NetworkID=rep("N1_node",nrow(temp2$templatepoints) ), temp2$templatepoints)
      myout<-rbind(myout,subnet)
      } else {
      pre<-paste("N",x-1,sep='')
      mynextout<-data.frame( NetworkID=paste(pre,"_omnibus",sep=''),temp$templatepoints)
      subnet<-data.frame( NetworkID=rep(paste(pre,"_node",sep=''),nrow(temp2$templatepoints) ), temp2$templatepoints)
      myout<-rbind(myout,mynextout)
      myout<-rbind(myout,subnet)
      }
  }
  return( myout )
}


