\name{timeseriesN3}
\alias{timeseriesN3}

\title{
Run N3 on slices of timeseries.
}

\description{
Repeatedly run N3 on slices of time series and return image
}

\usage{
tsn3<-timeseriesN3( mat, mask, c(4,4,2)  )
}

\arguments{
\item{mat}{
  input matrix
}
\item{mask}{
  mask to use
}
\item{ncorrections}{
  levels at which to apply n3
}
}

\value{
antsImage is output
}

\author{
Avants BB
}

\examples{
  bold<-makeImage( c(10,10,10,4), rnorm(4000) )
  mask<-getMask(bold)
  boldn3<-timeseriesN3( bold, mask, c(4,2) )
}
