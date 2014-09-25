\name{SmoothImage}
\alias{SmoothImage}

\title{
Smooth an image
}

\description{
Perform smoothing on the given image with a given sigma, defined in physical space units
}

\usage{
SmoothImage( "<imageDimension>" , "<inputImage>" , "SigmaVector" , "<outputimage>")
}

\arguments{
\item{imageDimension 2|3|4}{
Number of dimensions of the input image
}

\item{inputImage}{
Input image to operate on
}


\item{Sigma}{
Smoothing factor in format 1.1x1.5x0.5 for a 3D image
}

\item{outputImage}{
Result image
}

}

\value{
0 -- Success\cr
1 -- Failure
}

\author{
Shrinidhi KL, Avants BB
}

\examples{
\dontrun{
# different ways you might smooth images of different dimensionality
SmoothImage( 2 , img ,"1.2x1.5", img)
SmoothImage( 3 , img ,"1x0x1", img)
SmoothImage( 4 , img ,"1x2x1x0", img)
}
}
