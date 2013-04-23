plotBasicNetwork <- function( centroids , brain  )
{
  if ( missing( centroids ) | missing( brain ) )
    {
    print( args( plotBasicNetwork ) )
    return(1)
    }
  nLabels <- nrow(centroids)
  rgl.bg(color="white")
  par3d(windowRect=c(100,100,600,600))  
  mesh<-getvertices( brain[[1]] )
  nSurfaceVerts <- dim(mesh$vertices)[1]
  mesh$vertices <- rbind( mesh$vertices,as.matrix( centroids ) )
  labelVerts <- c( 1:nrow(centroids) )+nSurfaceVerts
  spheres3d( mesh$vertices[labelVerts, ] , col='blue',type='s',radius=3)
  edgelocations <- c()
  for ( i in c(1:nLabels) )
    {
    for ( j in c(i:nLabels) )
      {
      edgelocations <- c(edgelocations, nSurfaceVerts+c(i,j) )
      }
    }  
  segments3d( mesh$vertices[edgelocations,], col="red", lwd=2)
  return( 1 )    
}
