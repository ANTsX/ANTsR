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
img <- antsImageRead( filename, 3 )
mask <- antsImageClone(img)
mask[ mask < 1 ] = 0
mask[ mask > 0 ] = 1
matrix <- antsGetNeighborhood(img, mask, radius)
}

}
