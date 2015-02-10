#' Rotate an existing 3d window into different views.
#'
#' The make3ViewPNG function rotates the existing viewport according to 3
#' different rotation matrices passed in by the user.  The output of these 3
#' views is munged together along the left/right edge and written to a png
#' file.
#'
#'
#' @param rotationView1 Leftmost view
#' @param rotationView2 Center view
#' @param rotationView3 Rightmost view
#' @param fnprefix Output file name prefix.
#' @return NA
#' @author Avants BB
#' @examples
#'
#' \dontrun{
#' brain<-renderSurfaceFunction( surfimg =list( bm ) ,
#'     alphasurf=0.1 ,smoothsval = 1.5 , smoothfval = 1.0,
#'     funcimg=list(cnt$clustimg) , alphafunc=0.2 )
#' plotBasicNetwork( centroids =  cnt$centroids , brain )
#' make3ViewPNG(  rid, id, rid2,  paste('figure/network',i,sep='') )
#' }
#'
#' @export make3ViewPNG
make3ViewPNG <- function(rotationView1, rotationView2, rotationView3, fnprefix) {
  if ( !  usePkg("rgl")  |
       !  usePkg("grid") |
       !  usePkg("png") )
       {
       print("Need rgl, grid and png")
       return(NULL)
       }
  if (nargs() == 0 | missing(fnprefix)) {
    print("Usage:  make3ViewPNG( x , fn = 'output.png') ")
    return(1)
  }
  id <- rgl::par3d("userMatrix")
  if (missing(rotationView1)) {
    rotationView1 <- rgl::rotate3d(id, -pi/2, 1, 0, 0)
  }
  if (missing(rotationView2)) {
    rotationView2 <- rgl::rotate3d(id, pi/2, 0, 0, 1)
  }
  if (missing(rotationView3)) {
    rotationView3 <- rgl::rotate3d(id, -pi/2, 0, 0, 1)
  }
  rgl::par3d(userMatrix = rotationView1)
  rgl::rgl.snapshot(paste(fnprefix, "a.png", sep = ""), fmt = "png", top = TRUE)
  rgl::par3d(userMatrix = rotationView2)
  rgl::rgl.snapshot(paste(fnprefix, "b.png", sep = ""), fmt = "png", top = TRUE)
  rgl::par3d(userMatrix = rotationView3)
  rgl::rgl.snapshot(paste(fnprefix, "c.png", sep = ""), fmt = "png", top = TRUE)
  aa <- png::readPNG(paste(fnprefix, "a.png", sep = ""))
  bb <- png::readPNG(paste(fnprefix, "b.png", sep = ""))
  cc <- png::readPNG(paste(fnprefix, "c.png", sep = ""))
  aabbcc <- abind::abind(aa, bb, along = 2)
  aabbcc <- abind::abind(aabbcc, cc, along = 2)
  png(paste(fnprefix, ".png", sep = ""), width = dim(aabbcc)[2], height = dim(aabbcc)[1])
  grid::grid.raster(aabbcc)
  dev.off()
  return(aabbcc)
}
