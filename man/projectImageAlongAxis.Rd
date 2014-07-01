\name{projectImageAlongAxis}
\alias{projectImageAlongAxis}

\title{
Simple projectImageAlongAxis function.
}

\description{
Will turn an N-D image into an N-1-D image by summation (0), max-intensity (1) or min-intensity (2) projection along an orthogonal axis.
}

\usage{
proj<-projectImageAlongAxis(  img4d, refimg3d, projtype=0,  axis=3 )
}

\arguments{
\item{img4d}{
  input image
}
\item{refimg3d}{
  down-dimensional image to help define physical space 
}
\item{projtype}{
  projection type 0, 1 or 2
}
\item{axis}{
  should be less than image dimension and greater than or equal to 0
}
}

\value{
image of n-dimensions-1 is output
}

\author{
Avants BB
}

\examples{
\dontrun{
mask4dproj<-projectImageAlongAxis( mask4d, mask3d, axis=3, projtype=0 )
}
}
