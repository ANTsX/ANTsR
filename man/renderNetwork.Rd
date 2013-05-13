\name{renderNetwork}
\alias{renderNetwork}

\title{
3D surface-based rendering of image ROI-based networks
}

\description{
Will use rgl to render a spatial graph  
}

\usage{
renderNetwork<-function( network, locations, scaling=c(0,0), lwd=2, radius=3, edgecolors=0, nodecolors='blue', nodetype='s' )
}

\arguments{
\item{network}{
NxN array or matrix of network connectivity values.
}

\item{locations}{
Nx3 array or matrix of node locations
}

\item{scaling}{
min and max values to use for coloring of edge values
}

\item{lwd}{
width of edge lines
}

\item{edgecolors}{
pre-defined colors for graph edges
}

\item{radius}{
radius of nodes
}

\item{nodetype}{
type of glyph for nodes
}

}

\value{
0 -- Success\cr
1 -- Failure
}

\author{
Duda, J
}

\examples{
\dontrun{
renderNetwork(network, locations)
renderNetwork(network, locations, lwd=3, edgecolors=heat.colors(100) )
}
}
