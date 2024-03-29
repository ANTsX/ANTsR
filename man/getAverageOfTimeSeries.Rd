% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getAverageOfTimeSeries.R
\name{getAverageOfTimeSeries}
\alias{getAverageOfTimeSeries}
\title{getAverageOfTimeSeries}
\usage{
getAverageOfTimeSeries(img)
}
\arguments{
\item{img}{input 4D image}
}
\value{
3D ants image is output
}
\description{
Returns a 3D average of a 4D time series.
}
\examples{
img <- as.antsImage(array(data = rep(0, 10^4), dim = c(10, 10, 10, 10)))
avg <- getAverageOfTimeSeries(img)
}
\author{
Avants BB
}
