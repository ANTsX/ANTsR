# renderNetwork
renderNetwork <- function( network, locations, scaling=c(0,0)  )
{

  nLabels <- dim(locations)[1]
  network <- as.array(network)
  
  if ( length(dim(network)) != 2 ) {
    stop( "network must have exactly 2 dimensions" )
  }
  if ( (dim(network)[1] != dim(locations)[1]) ||
       (dim(network)[2] != dim(locations)[1]) )
    {
      stop( "network and centroids must have matching sizes" )
    }

  mesh <- list( vertices=locations )
  labelVerts <- c( 1:nLabels )
  spheres3d( mesh$vertices[labelVerts, ], col='blue',type='s',radius=3)
  
  edgelocations <- c()
  edgeweights <- c()
        
  for ( i in c(1:nLabels) )
    {
    for ( j in c(i:nLabels) )
      {
      if (network[i,j] != 0 )
        {
        edgelocations <- c(edgelocations, c(i,j) )
        edgeweights <- c(edgeweights, network[i,j] )
        }
      }
    }

  if ( (scaling[1] == scaling[2] ) )
    {
    scaling[1] <- min( edgeweights )
    scaling[2] <- max( edgeweights ) 
    }
  
  edgeweights <- edgeweights - scaling[1]
  edgeweights <- edgeweights / scaling[2]
  
  edgeweights <- 1+(edgeweights * nLabels)
  heat <- heat.colors(nLabels+1)
  colors <- heat
  for ( i in c(1:length(edgeweights) ) )
    {
    colors[i] <- heat[floor(edgeweights[i])]
    }
  #print( "edge colors determined" )
  
  segments3d( mesh$vertices[edgelocations,], col=rep(colors,each=2), lwd=7)
}
