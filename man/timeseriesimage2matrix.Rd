\name{timeseriesimage2matrix}
\alias{timeseriesimage2matrix}

\title{
Time-series image to matrix
}

\description{
Extract a matrix from a time-series image after applying a mask.
}

\usage{
timeseriesimage2matrix( img , mask )
}

\arguments{
\item{img}{
Input image of type 'antsImage' or R array.
}
\item{mask}{
  Input mask of type 'antsImage' or R array. In either case, the number
  of voxels in the mask may be either equal to that of input image 'img'
  or equal to number of voxels in one time unit of input image 'img'. In
  the second case, the mask is reused for every time unit of the 'img'.
  }
}

\value{
Success -- an R matrix of dimensions ( dim(img)[length(dim(img))] , sum(mask==1) )\cr
}

\author{
Shrinidhi KL
}

\examples{
\dontrun{
img <- antsImageRead( "input.nii" , 4 )
mask <- array( 1 , dim(img)[1:3] )
# set some values of mask to FALSE ...

mat <- timeseriesimage2matrix( img , mask )
}
}
