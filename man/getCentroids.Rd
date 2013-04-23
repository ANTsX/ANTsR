\name{getCentroids}
\alias{getCentroids}

\title{
Convert an image to the geometric centroids of its signal
}

\description{
Reduces a variate/statistical/network image to a set of centroids describing the center of each stand-alone non-zero component in the image.
}

\usage{
centroids<-getCentroids(  img  )
}

\arguments{
\item{img}{
the image to reduce to centroids - presumably some kind of statistical or network map  
}
\item{threshparam}{
  thresholds the input image at  mean( img[ img > 0 ] ) - threshparam * sdev( img[ img > 0 ] )
}
\item{clustparam}{
  ignore post-threshold clusters smaller than this value 
}
}

\value{
the centroids are output in matrix of size npoints by 3 
}

\author{
Avants BB
}

\examples{
\dontrun{
 getCentroids( f  , clustparam = 250, threshparam = 0 )
}
}
