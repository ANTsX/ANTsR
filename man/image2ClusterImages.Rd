\name{image2ClusterImages}
\alias{image2ClusterImages}

\title{
 Converts an image to several independent images.
}

\description{
 Produces a unique image for each connected component 1 through N of size > minClusterSize 
}

\usage{
clust<-image2ClusterImages( img , minClusterSize = 50 , minThresh = 1.e-6 , maxThresh = 1 )
}

\arguments{
\item{img}{
  input antsImage e.g. a statistical map 
}
}

\value{
the original image broken into a list of cluster images is the output
}

\author{
Avants BB
}

\examples{
\dontrun{
imageclusterlist<-image2ClusterImages( img ) 
}
}
