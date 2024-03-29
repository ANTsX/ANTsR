% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preprocessRestingBOLD.R
\name{computeDVARSspatialMap}
\alias{computeDVARSspatialMap}
\title{computeDVARSspatialMap}
\usage{
computeDVARSspatialMap(boldMatrix)
}
\arguments{
\item{boldMatrix}{matrix of bold signal}
}
\value{
DVARS spatial vector.
}
\description{
compute the DVARS quality control metric at every voxel
}
\examples{

mat <- matrix(c(0, 1, 2, 0, 0, 1, 2, 2, 2), ncol = 3)
dv <- computeDVARSspatialMap(mat)

}
\author{
Tustison NJ, Avants BB
}
