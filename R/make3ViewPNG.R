make3ViewPNG <- function( rotationView1 , rotationView2 , rotationView3, fnprefix )
  {
  library(rgl)
  library(grid)
  library(png)
  if ( nargs() == 0 | missing( fnprefix ) )
    {
    print("Usage:  make3ViewPNG( x , fn = 'output.png') ")
    return(1)
    }
  id<-par3d("userMatrix")
  if ( missing( rotationView1 ) )
    {
    rotationView1<-rotate3d( id , -pi/2, 1, 0, 0 )
    }
  if ( missing( rotationView2 ) )
    {
    rotationView2<-rotate3d( id , pi/2, 0, 0, 1 )
    }
  if ( missing( rotationView3 ) )
    {
    rotationView3<-rotate3d( id , -pi/2, 0, 0, 1 )
    }
  par3d(userMatrix = rotationView1 )
  rgl.snapshot( paste(fnprefix,'a.png',sep='') , fmt="png", top=TRUE )
  par3d(userMatrix = rotationView2 )
  rgl.snapshot( paste(fnprefix,'b.png',sep='') , fmt="png", top=TRUE )
  par3d(userMatrix = rotationView3 )
  rgl.snapshot( paste(fnprefix,'c.png',sep='') , fmt="png", top=TRUE )
  aa<-readPNG( paste(fnprefix,'a.png',sep='') )
  bb<-readPNG( paste(fnprefix,'b.png',sep='') )
  cc<-readPNG( paste(fnprefix,'c.png',sep='') )
  aabbcc<-abind(aa,bb,along=2)
  aabbcc<-abind(aabbcc,cc,along=2)
  png(  paste(fnprefix,'.png',sep='')  , width=dim(aabbcc)[2], height=dim(aabbcc)[1] )
  grid.raster(aabbcc)
  dev.off()
  return( aabbcc )
  }
