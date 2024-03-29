% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeGraph.R
\name{makeGraph}
\alias{makeGraph}
\title{Simple function to create and measure a graph from a square input matrix.}
\usage{
makeGraph(
  mat,
  graphdensity = 1,
  communityMethod = NA,
  getEfficiency = FALSE,
  inverseValuesAsWeights = FALSE
)
}
\arguments{
\item{mat}{input matrix}

\item{graphdensity}{fraction of edges to keep}

\item{communityMethod}{see igraph's community detection}

\item{getEfficiency}{boolean, this is slow to compute}

\item{inverseValuesAsWeights}{if TRUE, high correlations produce small
edge weights.  This detail is important when using igraph algorithms as
different methods use weights in different ways. See igraph for its
\code{is_weighted} documentation which notes that weights are used as
distances in shortest path calculations and as strength (similar to degree)
for community methods.}
}
\value{
a named list is output including the graph object, adjacency matrix
and several graph metrics
}
\description{
Creates an igraph object from a square input correlation matrix - only
positive correlations are used.  Based on the graph.adjacency function of
igraph.  gplot is helpful for visualization.
}
\examples{
mat <- matrix(c(1, 0.5, 0.2, -0.1, 1, 0.3, -0.2, 0.6, 1), ncol = 3)
gobj <- makeGraph(mat, 0.5)

}
\author{
Avants BB
}
