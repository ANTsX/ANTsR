\name{getAverageOfTimeSeries}
\alias{getAverageOfTimeSeries}

\title{
Returns a 3D average of a 4D time series.
}

\description{
Averages the 3D sub-volumes of a 4D input ants image.
}

\usage{
avg<-getAverageOfTimeSeries( img  )
}

\arguments{
\item{img}{
  input 4D image
}
}

\value{
3D ants image is output
}

\author{
Avants BB
}

\examples{
img<-as.antsImage( array(data = rep(0,10^4), dim = c(10,10,10,10) ) )
avg<-getAverageOfTimeSeries( img ) 
print(dim(avg))
print(avg@pixeltype)
avg<-getAverageOfTimeSeries( avg ) 
}
