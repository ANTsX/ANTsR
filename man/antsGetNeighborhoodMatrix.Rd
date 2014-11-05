\name{antsGetNeighborhoodMatrix}
\alias{antsGetNeighborhoodMatrix}

\title{
Get Neighborhood
}

\description{
Summarize neighborhoods for voxels in an antsImage.
}

\usage{
antsGetNeighborhood( image, mask, radius )
}

\arguments{
\item{image}{
Image object of S4 class 'antsImage' to get values from.
}

\item{mask}{
Image object of S4 class 'antsImage' indicating which voxels to examine
}

\item{radius}{
array of values for neighborhood radius (in voxels)
}

}

\value{
matrix of pixel values where the number of rows is the size of the neighborhood and there is a column for each voxel
}

\author{
Duda JT
}

\examples{
\dontrun{
mnit<-getANTsRData("mni")
mnit<-antsImageRead(mnit,3)
mask<-getMask(mnit,lowThresh=mean(mnit),cleanup=TRUE)
center <- dim(mnit)/2
radius <- rep(5,mnit@dimension)
mat<-antsGetNeighborhoodMatrix(mnit,center,radius)
}

}
