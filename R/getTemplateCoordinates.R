getTemplateCoordinates <- function( imagePairToBeLabeled, templatePairWithLabels , labelnames = NA , outprefix = NA , convertToTal = FALSE )
  {
  if ( nargs() == 0 | length(imagePairToBeLabeled) < 2
      | length(templatePairWithLabels) < 2 )
    {
    print( args( getTalairachCoordinates  ) )
    print( " imagePairToBeLabeled <-list( myBrain, myBrainBlobs ) ")
    print( " templatePairWithLabels <-list( ch2orMNI_Brain, ch2orMNI_BrodmannLabels ) ")
    print( " labelnames <-c(\"HippocampusL\",\"HippocampusR\") ")
    return(1)
    }
  fi<-templatePairWithLabels[[1]]
  mi<-imagePairToBeLabeled[[1]]
  if ( class(fi)[[1]] != "antsImage" )
    {
    print( "  class(fi)[[1]] != antsImage " )
    }
  if ( class(mi)[[1]] != "antsImage" )
    {
    print( "  class(mi)[[1]] != antsImage " )
    }
  imagedim<-mi@dimension
  if ( is.na( outprefix ) )
    {
    outprefix<-paste(tempdir(),"/Z",sep='')
    }
  txfn<-paste(outprefix,"0GenericAffine.mat",sep='')
  if ( ! file.exists( txfn ) )
    mytx<-antsRegistration(fixed=fi , moving=mi , typeofTransform = c("Affine"), outprefix=outprefix )  else mytx<-list(fwdtransforms=txfn )
  mywarpedimage<-antsApplyTransforms(fixed=fi,moving=mi,transformlist=mytx$fwdtransforms, interpolator=c("Linear") )
  milab<-imagePairToBeLabeled[[2]]
  mywarpedLimage<-antsApplyTransforms(fixed=fi,moving=milab,transformlist=mytx$fwdtransforms, interpolator=c("NearestNeighbor") )
  pointfile<-paste(outprefix,"coords.csv",sep='')
  ImageMath( milab@dimension , pointfile , "LabelStats", mywarpedLimage , 1 )
  mypoints<-read.csv(pointfile)
  for ( mylab in 2:length(templatePairWithLabels) )
    {
    filab<-templatePairWithLabels[[mylab]]
    if ( class(filab)[[1]] != "antsImage" )
    {
    print( "  class(filab)[[1]] != antsImage " )
    }
    if ( class(milab)[[1]] != "antsImage" )
    {
    print( "  class(milab)[[1]] != antsImage " )
    }
    # now we know the (e.g. MNI) coordinate of each labeled region in the original image
    # we want, next, to identify the 'Brodmann' label for each of these regions
    # for instance, you might have found 2 blobs, blob1 and blob2 with labels 1 and 2
    # you know want to know if these are at Brodmann area 21 or 22 or whatever
    # so we iterate through the point list and index the filab image ( template labels )
    templateLab<-rep( NA, nrow( mypoints ) ) 
    for ( i in 1:nrow(mypoints) )
      {
      if ( imagedim == 2) myargs<-list( imagedim , "NA", "SetOrGetPixel",filab,"Get",mypoints[i,1] ,mypoints[i,2],"1") 
      if ( imagedim == 3) myargs<-list( imagedim , "NA", "SetOrGetPixel",filab,"Get",mypoints[i,1] ,mypoints[i,2], mypoints[i,3],"1") 
      myval<-capture.output(  .Call( "ImageMath", int_antsProcessArguments( c(myargs) ) ) )
      templateLab[i]<-myval[1]# getValueAtPoint( filab, c( mypoints[i,1:imagedim] ) )
      }
    mypoints<-cbind( mypoints, templateLab = templateLab )
    }
  if ( convertToTal & imagedim == 3 )
    {
    talpoints<-mypoints
    for ( i in 1:nrow(talpoints) )
      {
      talpoints[i,1:3]<-mni2tal( talpoints[i,1:3] )
      }
    return( list( templatepoints=mypoints, talpoints=talpoints , myLabelsInTemplateSpace=mywarpedLimage,  myImageInTemplateSpace=mywarpedimage ) )
    }
  return( list( templatepoints=mypoints, myLabelsInTemplateSpace=mywarpedLimage,  myImageInTemplateSpace=mywarpedimage ) )
  }
