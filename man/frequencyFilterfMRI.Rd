\name{frequencyFilterfMRI}
\alias{frequencyFilterfMRI}

\title{
Band pass filtering for BOLD image.
}

\description{
This function works for a BOLD time-series data
}

\usage{
frequencyFilterfMRI <- function( boldmat, tr, freqLo = 0.01, freqHi = 0.1 , opt="trig" )
}

\arguments{
\item{boldMatrix}{Time series matrix for bold image}
\item{tr = <value>}{The sequence's TR value , typically 3 or 4.}
\item{freqLo = <value>}{The lower frequency limit, e.g. 0.01 in band-pass filter}
\item{freqHi = <value>}{The higher frequency limit, e.g. 0.1 in band-pass filter}
\item{opt = "trig"}{Type of filter to use.}
}

\value{
output is the filtered time series.
}

\author{
Avants BB
}

\examples{
\dontrun{
fmat<-timeseries2matrix( img, mask )
myres<-frequencyFilterfMRI( fmat, tr = 4, freqLo = 0.01, freqHi = 0.1, opt = "trig" )
}
}
