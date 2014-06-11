\name{plotPrettyGraph}
\alias{plotPrettyGraph}

\title{
Simple plotPrettyGraph function saves to png.
}

\description{
PlotPrettyGraph given inputs from the makeGraph function.  adapted from http://is-r.tumblr.com/.
}

\usage{
plotPrettyGraph( graphObject, adjacencyMatrix, graphMetricValue, pngfn="graph.png", scaleText=0.5, vertexSize = NA, figScale=11 , layoutmode = "eigen" )
}

\arguments{
\item{graphObject}{
  igraph graphObject
}
\item{adjacencyMatrix}{
  igraph adjacencyMatrix
}
\item{graphMetricValue}{
  igraph node-level graph value e.g. degree, page.rank, etc
}
\item{pngfn}{
  filename for output png
}
\item{scaleText}{
  relative size of text to vertices
}
\item{vertexSize}{
  cex size of vertices
}
\item{figScale}{
  the figure will be of square size 2^figScale in pixels
}
\item{layoutmode}{
  see gplot.layout in sna package
}
}

\value{
no output
}

\author{
Avants BB, Christopher DeSante and David Sparks
}

\examples{
data("bold_correlation_matrix",package="ANTsR")
dmat<-data.matrix(bold_correlation_matrix)
gg<-makeGraph( dmat, 0.1 )
rownames(gg$adjacencyMatrix)<-colnames(bold_correlation_matrix)
plotPrettyGraph( gg$mygraph, gg$adjacencyMatrix, gg$degree , figScale=12 , scaleText=5 )
}
