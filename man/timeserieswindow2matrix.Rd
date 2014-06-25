\name{timeserieswindow2matrix}
\alias{timeserieswindow2matrix}

\title{
Time-series image to matrix of time windows around user-specified events
}

\description{
Extract a matrix from a time-series image after applying a mask where each row is a space-time vector
}

\usage{
timeserieswindow2matrix( timeseriesmatrix , mask, eventlist, timewindow, zeropadvalue, spacing=NA )
}

\arguments{
\item{timeseriesmatrix}{
Input timeseriesmatrix from timeseries2matrix
}
\item{mask}{
  Input mask of type 'antsImage' ... the mask will be replicated into a 4D image of length timewindow + zeropadvalue.
  }
\item{eventlist}{
  time indices for the events to extract
}
\item{timewindow}{
  n-timepoints around each event, forward in time.
}
\item{zeropadvalue}{
  pads the mask by this amount fwd and bwd in time.
}
\item{spacing}{
  optional 4d spacing vector that will impact smoothing parameters
}
}

\value{
Success -- an R matrix of dimensions ntimewindow*sizeofnonzerovaluesinmask*nevents \cr
}

\author{
Avants BB
}

\examples{
img <- makeImage( c(10,10,10,50) , 0 )
mask <- as.antsImage( array( 1 , dim(img)[1:3] ) )
mat<-timeseries2matrix( img, mask )
mat <- timeserieswindow2matrix( mat , mask, c(1, 4, 5 ), 2, 0 )
print( dim(mat$eventmatrix) )
print( dim(mat$mask4d) )
}
