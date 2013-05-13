renderImageLabels<-function( labelsimg, surfval=0.5, smoothsval = 0, blobrender = TRUE , alphasurf=1 , alphafunc=1, outdir="./", outfn=NA, physical=TRUE, color=c(), labels=FALSE )
  {
  if ( missing(labelsimg) )
    {
    stop('Check usage:  at minimum, you need to call \n renderSurfaceFunction( an_ants_image ) \n ')
    }

  nLabels <- max(as.array(labelsimg))

  colors <- color
  if ( length(colors) < 1 ) {
    colors <- snapColors(nLabels)
  }
  mylist <- list()
  
  for ( i in 1:nLabels )
    {
    print (  i )
    limg <- antsImageClone( labelsimg )
    ThresholdImage(3, limg, limg, i, i )
    
    if (smoothsval > 0) {
       SmoothImage(3, limg, smoothsval, limg)
    }

    print( sum(as.array(limg)))
    
    surf<-as.array( limg )
    brain <- contour3d(surf, level=surfval, alpha=alphasurf, draw=FALSE, smooth=1, color=colors[i])
    
    print( "convert points" )
    if (physical == TRUE ){
      brain$v1 <-  antsTransformIndexToPhysicalPoint(limg, brain$v1)
      brain$v2 <-  antsTransformIndexToPhysicalPoint(limg, brain$v2)
      brain$v3 <-  antsTransformIndexToPhysicalPoint(limg, brain$v3)
    } 
    mylist[[i]] <- brain
  }

  drawScene.rgl(mylist,add=TRUE)
  return(mylist)
}
