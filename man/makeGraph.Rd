\name{makeGraph}
\alias{makeGraph}

\title{
Simple function to create and measure a graph from a square input matrix.
}

\description{
Creates an igraph object from a square input correlation matrix - only positive correlations are used.  Based on the graph.adjacency function of igraph.  gplot is helpful for visualization.
}

\usage{
wmat<-makeGraph( mat , graphdensity=0.5 )
}

\arguments{
\item{mat}{
  input matrix
}
\item{graphdensity}{
  fraction of edges to keep
}
}

\value{
a named list is output including the graph object, adjacency matrix and several graph metrics
}

\author{
Avants BB
}

\examples{
\dontrun{
mat <- matrix(c(rep(1,100)),ncol=10)
gobj<-makeGraph( mat ) 
mat <- matrix(  c( 1, 0.5, 0.2, -0.1, 1, 0.3, -0.2, 0.6, 1 ) , ncol= 3 )
gobj<-makeGraph( mat , 0.5 ) 
gplot( dd$adjacencyMatrix )
}
}
