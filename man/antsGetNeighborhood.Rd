\name{antsGetNeighborhood}
\alias{antsGetNeighborhood}

\title{
Get Neighborhood
}

\description{
Get the values in a local neighborhood of an 'antsImage'.
}

\usage{
antsGetNeighborhood( image, center, radius )
}

\arguments{
\item{image}{
Image object of S4 class 'antsImage' to get values from.
}

\item{center}{
array of indices for neighborhood center
}

\item{radius}{
array of values for neighborhood radius (in voxels)
}

}

\value{
array of pixel values in neighborhood
}

\author{
Duda JT
}

\examples{
\dontrun{
mnit<-getANTsRData("mni")
mnit<-antsImageRead(mnit,3)
center <- dim(mnit)/2
radius <- rep(5,mnit@dimension)
mat<-antsGetNeighborhood(mnit,center,radius)
}

}
