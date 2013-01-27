renderSurfaceFunction<-function( surfimg, funcimg, surfval=0.5, basefval , offsetfval , smoothsval = 0, smoothfval = 0, blobrender = TRUE )
  {
  if ( missing(surfimg) )
    {
    cat('Check usage:  at minimum, you need to call \n renderSurfaceFunction( an_ants_image ) \n ')
    return(NULL)
    }
  tryCatch(
  if ( smoothsval > 0 ) {
    SmoothImage(3,surfimg,smoothsval,surfimg)
  }
  )
  if ( missing( funcimg ) )
    {  cat("just the surface\n")
    surf<-as.array( surfimg )
    brain <- contour3d(  surf , level = c(surfval), alpha = 1,draw=FALSE,smooth=1,material="metal",depth=0.25,color="white")
    drawScene.rgl(list(brain))
    return(NULL)
    }
  if ( smoothfval > 0 ) {
    SmoothImage(3,funcimg,smoothfval,funcimg)
  }
  if ( missing( basefval ) )
    { # just threshold at mean > 0 
    basefval<-mean( funcimg[ funcimg > 0 ] )
    }
  if ( missing( offsetfval ) ) offsetfval<-sd( funcimg[ funcimg > basefval ] )
  print(paste("will render baseval:",basefval,"offsetval:",offsetfval))
  blobi<-as.array( funcimg )
  surf<-as.array( surfimg )
  print("get substrate contour")
  brain <- contour3d(  surf , level = c(surfval), alpha = 1,draw=FALSE,smooth=1,material="metal",depth=0.2,color="white")
  print("get function contour1")
  blob <- contour3d(  surf , level = c(surfval), mask=( blobi > (basefval+offsetfval) ), alpha= 1, draw=FALSE,smooth=1,material="metal",depth=0.5,color="red")
  print("get function contour2")
  blob1 <- contour3d(  surf , level = c(surfval), mask=( blobi > (basefval)), alpha= 1, draw=FALSE,smooth=1,material="metal",depth=0.5,color="yellow")
  if ( blobrender ) 
    {
    print("prep render")
    lev<-c(basefval,basefval+offsetfval,basefval+offsetfval*2)
    blob2 <- contour3d( blobi , level = lev , alpha = c(0.3,0.6,0.9), draw= FALSE,smooth=1,color=heat.colors(n=length(lev)))
    drawScene.rgl(list(blob2,blob,blob1,brain))
    } else drawScene.rgl(list(blob,blob1,brain)) # surface render 
# movie3d(spin3d(),duration=10)
}
