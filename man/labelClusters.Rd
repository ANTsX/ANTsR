% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/labelClusters.R
\name{labelClusters}
\alias{labelClusters}
\title{Simple labelClustering function.}
\usage{
labelClusters(
  imagein,
  minClusterSize = 50,
  minThresh = 1e-06,
  maxThresh = 1,
  fullyConnected = FALSE
)
}
\arguments{
\item{imagein}{input antsImage e.g. a statistical map}

\item{minClusterSize}{throw away clusters smaller than this value}

\item{minThresh}{threshold to a statistical map}

\item{maxThresh}{threshold to a statistical map}

\item{fullyConnected}{boolean sets neighborhood connectivity pattern}
}
\value{
labeled cluster image is output
}
\description{
This will give a unique ID to each connected component 1 through N of size >
minClusterSize
}
\examples{

img <- antsImageRead(getANTsRData("r16"))
timgFully <- labelClusters(img, 10, 128, 150, TRUE)
timgFace <- labelClusters(img, 10, 128, 150, FALSE)
\dontrun{
plot(img, timgFace)
plot(img, timgFully)
}

}
\author{
Avants BB
}
