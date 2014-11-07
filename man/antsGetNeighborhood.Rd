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

\item{physical.coordinates}{
a logical indicating if voxel indices and offsets should be in voxel or physical coordinates
}

}

\value{
list containing three matrices:
values: matrix of pixel values where the number of rows is the size of the neighborhood and there is a column for each voxel
indices: matrix providing the coordinates for each value
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
