plotBasicNetwork <- function( mask , centroids  )
{
  if ( missing( mask ) | missing( centroids ) )
    {
    print( args( plotBasicNetwork ) )
    return(1)
    }
       
  nLabels <- nrow(centroids)
  open3d()
  rgl.bg(color="white")
  surf<-as.array( mask )
  brain <- contour3d(  surf , level = c(0.5), alpha = 0.1,draw=FALSE,smooth=1,material="metal",depth=0.6,color="white")
  # convert to physical space
  brain$v1 <- antsTransformIndexToPhysicalPoint( mask, brain$v1 )
  brain$v2 <- antsTransformIndexToPhysicalPoint( mask, brain$v2 )
  brain$v3 <- antsTransformIndexToPhysicalPoint( mask, brain$v3 )
  drawScene.rgl(list(brain))
  par3d(windowRect=c(100,100,600,600))  
  mesh<-getvertices( brain )
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
