\name{frequencyFilterfMRI}
\alias{frequencyFilterfMRI}

\title{
Band pass filtering for BOLD image.
}

\description{
This function works for a BOLD time-series data
}

\usage{
frequencyFilterfMRI <- function( boldmat, tr, freqLo = 0.01, freqHi = 0.1 , opt=c("trig","stl","butt") )
}

\arguments{
\item{boldMatrix}{Time series matrix for bold image}
\item{tr = <value>}{The sequence's TR value , typically 3 or 4.}
\item{freqLo = <value>}{The lower frequency limit, e.g. 0.01 in band-pass filter}
\item{freqHi = <value>}{The higher frequency limit, e.g. 0.1 in band-pass filter}
\item{opt = c("trig","butt","stl")}{Type of filter to use: butterworth, trigonometric, stl.}
}

\value{
output is the filtered time series.
}

\author{
Avants BB
}

\examples{
fmat<-replicate(1000, rnorm(200))
k<-1
for ( ftype in c("butt","stl","trig") ) {
  myres<-frequencyFilterfMRI( fmat, tr = 4, freqLo = 0.01, freqHi = 0.05, opt = ftype )
  comparemat<-cbind(fmat[,k],myres[,k])
  plot(ts(comparemat),main=ftype)
  Sys.sleep(0.3)
}
}
