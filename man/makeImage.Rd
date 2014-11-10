\name{makeImage}
\alias{makeImage}

\title{
Simple makeImage function.
}

\description{
Make an image with given size and voxel value or given a mask and vector.
}

\usage{
img<-makeImage( c(2,10) , 1   )
img<-makeImage( mask , vec   )
}

\arguments{
\item{mat}{
  input image size or mask
}
\item{val}{
  input image value or vector, size of mask
}
}

\value{
antsImage is output
}

\author{
Avants BB
}

\examples{
outimg<-makeImage( c(2,10) , 1)
outimg<-makeImage( outimg ,  c(2,10) )
}
