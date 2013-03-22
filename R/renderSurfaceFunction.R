renderSurfaceFunction<-function( surfimg, funcimg, surfval=0.5, basefval , offsetfval , smoothsval = 0, smoothfval = 0, blobrender = TRUE , alphasurf=1 , alphafunc=1, outdir="./", outfn="movie", mycol, physical=FALSE )
  {
  if ( missing( mycol ) )
    {
    mycol<-rainbow(length(surfimg))
    }
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
    brain <- contour3d(  surf , level = c(surfval), alpha = alphasurf,draw=FALSE,smooth=1,material="metal",depth=0.6,color="white")
    # each point has an ID, 3 points make a triangle , the points are laid out as
    # c( x1 , y1, z1, x2, y2, z2 , ... , xn, yn, zn )
    # indices are just numbers
#    vertices<-c(  
#        brain <-  subdivision3d(brain)
 
    if (physical == TRUE )
      {
      brain$v1 <-  antsTransformIndexToPhysicalPoint(surfimg, brain$v1)
      brain$v2 <-  antsTransformIndexToPhysicalPoint(surfimg, brain$v2)
      brain$v3 <-  antsTransformIndexToPhysicalPoint(surfimg, brain$v3)
      }
       
    drawScene.rgl(list(brain))
    return(list(brain))
    }
  if ( smoothfval > 0 ) {
    for ( i in 1:length(funcimg) ) {
      fimg<-antsImageClone( funcimg[[i]] )
      SmoothImage(3,fimg,smoothfval,fimg)
      funcimg[[i]]<-fimg
    }
  }
  surf<-as.array( surfimg )
  brain <- contour3d(  surf , level = c(surfval), alpha = alphasurf,draw=FALSE,smooth=1,material="metal",depth=0.6,color="white")
  if (physical == TRUE )
    {
    brain$v1 <-  antsTransformIndexToPhysicalPoint(surfimg, brain$v1)
    brain$v2 <-  antsTransformIndexToPhysicalPoint(surfimg, brain$v2)
    brain$v3 <-  antsTransformIndexToPhysicalPoint(surfimg, brain$v3)
    }
  mylist<-list(brain)
  for ( i in 1:length(funcimg) )
    {
      func<-as.array( funcimg[[i]] )
      vals<-abs( funcimg[[i]][ funcimg[[i]] > 0 ] )
      if ( missing( basefval ) )
        { # just threshold at mean > 0
          usefval<-mean(vals)
          print(usefval)
        } else usefval<-basefval
      if ( missing( offsetfval ) ) offsetfval<-sd( vals[ vals > usefval ] )
      print( paste( i , usefval )  )
      blob <- contour3d(  func , level = c(usefval), alpha = alphafunc,draw=FALSE,smooth=1,material="metal",depth=0.6,color=mycol[[i]])
      if (physical == TRUE )
        {
        blob$v1 <-  antsTransformIndexToPhysicalPoint(funcimg, blob$v1)
        blob$v2 <-  antsTransformIndexToPhysicalPoint(funcimg, blob$v2)
        blob$v3 <-  antsTransformIndexToPhysicalPoint(funcimg, blob$v3)
        }
      mylist<-lappend(mylist,blob)
    }
#  s<-scene3d()
#  s$par3d$windowRect <- c(0, 0, 500, 500) # make the window large 1.5*s$par3d$windowRect
#  s$par3d$zoom = 1.1 # larger values make the image smaller
  drawScene.rgl(mylist) # surface render 
  par3d(windowRect = c(0, 0, 500, 500)) # make the window large
  par3d(zoom = 1.1) # larger values make the image smaller
  drawScene.rgl(mylist) # surface render 
  movie3d(spin3d(),duration=10,dir=outdir, movie=outfn,clean=F)
  return( mylist ) 
}




#Make a function that will make each facet from data returned from surfaceTriangles applied to a function
#(probably a more elegant way to do this?)
makefacet<-function(data){
  #Code for 3D function->stl files for molding and casting
#stl creation functions similar to week 4 files
#Laura Perovich Oct 2012
#Load package misc3d that includes surfaceTriangles function
#Define character constants used in the stl files
tristart1<-"facet normal 0 0 0"
tristart2<-" outer loop"
triend1<-" endloop"
triend2<-"endfacet"
startline1<-"+"
startline2<-" solid LAURA"
endline<-" endsolid LAURA"

facetvector<-c()
progress <- txtProgressBar( min = 0, max = nrow(data[[1]]), style = 3 )
for (i in 1:nrow(data[[1]])){
v1<-paste("  vertex", as.character(data[[1]][i, 1]), as.character(data[[1]][i, 2]), as.character(data[[1]][i, 3]), sep=" ")
v2<-paste("  vertex", as.character(data[[2]][i, 1]), as.character(data[[2]][i, 2]), as.character(data[[2]][i, 3]), sep=" ")
v3<-paste("  vertex", as.character(data[[3]][i, 1]), as.character(data[[3]][i, 2]), as.character(data[[3]][i, 3]), sep=" ")
facetvector<-c(facetvector, tristart1, tristart2, v1, v2, v3, triend1, triend2) 
  if( i %% 50 == 0 )
    {
    setTxtProgressBar( progress, i )
    }
}
return(facetvector)
}

#Make a function that puts the facets together with the file headers and writes it out
makestl<-function( facetvector, outfile ){
  #Code for 3D function->stl files for molding and casting
#stl creation functions similar to week 4 files
#Laura Perovich Oct 2012
#Load package misc3d that includes surfaceTriangles function
library(misc3d)
#Define character constants used in the stl files
tristart1<-"facet normal 0 0 0"
tristart2<-" outer loop"
triend1<-" endloop"
triend2<-"endfacet"
startline1<-"+"
startline2<-" solid LAURA"
endline<-" endsolid LAURA"
fileConn<-file(outfile)
myout<-c(startline1, startline2, facetvector, endline)
writeLines( myout, fileConn)
close(fileConn)
}
############################ to use this do ############################
# library(ANTsR)
# source('R/renderSurfaceFunction.R')
# fn<-"/Users/stnava/Downloads/resimplerenderingexample/wmss.nii.gz"
# img<-antsImageRead(fn,3)
# brain<-renderSurfaceFunction( img )
# fv<-makefacet(brain[[1]])
# makestl(fv,"/tmp/temp.stl")

getvertices<-function( inrglmesh )
  {
  cter<-nrow(inrglmesh[[1]])
  vertices <- matrix( NA, nrow=3 * cter, ncol=3)
  inds<-c(1:cter)
  vertices[ ( 3* inds - 2 ) , ]<-inrglmesh[[1]]
  vertices[ ( 3* inds - 1 ) , ]<-inrglmesh[[2]]
  vertices[ ( 3* inds - 0 ) , ]<-inrglmesh[[3]]
  indices<-rep(NA, nrow(vertices) )
  return( list( vertices= vertices , indices= indices ) )
  }
# vtri <- surfaceTriangles(vertices[,1], vertices[,2], vertices[,3] , color="red")
# drawScene(updateTriangles(vtri, material = "default", smooth = 3) )
