\name{labelClusters}
\alias{labelClusters}

\title{
Simple labelClustering function.
}

\description{
 This will give a unique ID to each connected component 1 through N of size > minClusterSize 
}

\usage{
clust<-labelClusters( img , minClusterSize = 50 , minThresh = 1.e-6 , maxThresh = 1 )
}

\arguments{
\item{img}{
  input antsImage e.g. a statistical map 
}
}

\value{
labeled cluster image is output
}

\author{
Avants BB
}

\examples{
\dontrun{
img<-antsImageRead( getANTsRData('mnib'), 3 )
outimage<-labelClusters( img ) 
}
}
