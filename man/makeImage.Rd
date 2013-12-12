\name{makeImage}
\alias{makeImage}

\title{
Simple makeImage function.
}

\description{
Make an image with given size and voxel value.
}

\usage{
img<-makeImage( c(2,10) , 1   )
}

\arguments{
\item{mat}{
  input image size
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
}
