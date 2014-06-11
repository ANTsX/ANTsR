\name{plotPrettyGraph}
\alias{plotPrettyGraph}

\title{
Simple plotPrettyGraph function saves to png.
}

\description{
PlotPrettyGraph given inputs from the makeGraph function.  adapted from http://is-r.tumblr.com/.
}

\usage{
plotPrettyGraph( graphObject, adjacencyMatrix, graphMetricValue  )
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
}

\value{
no output
}

\author{
Avants BB, Christopher DeSante and David Sparks
}

\examples{
data("bold_correlation_matrix",package="ANTsR")
gg<-makeGraph( data.matrix(bold_correlation_matrix), 0.1 )
rownames(gg$adjacencyMatrix)<-colnames(bold_correlation_matrix)
plotPrettyGraph( gg$mygraph, gg$adjacencyMatrix, gg$degree , figScale=12 , scaleText=5 )
}
