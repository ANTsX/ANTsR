\name{as.antsImage}
\alias{as.antsImage}

\title{
numeric matrix/array to antsImage
}

\description{
Create an \code{antsImage} object from a \code{numeric matrix} or \code{numeric array}.
}

\usage{
as.antsImage( object [, pixeltype] [, spacing] [, origin] )
}

\arguments{
\item{object}{
\code{numeric matrix} or \code{numeric array} to be used to construct the \code{antsImage}. Dimensions of this object are used to set the dimensions of the image constructed. Allowed dimensions: 2 , 3 , 4. Resulting image is filled up column-wise meaning that the first column of 'object' becomes the first column of the image and second column of 'object' becomes the second column of the image and so on.
}

\item{pixeltype}{
C++ datatype to be used to represent the pixels of the image constructed. Make sure you use a type that can represent all the values in the input vector. There may be truncation involved otherwise. Allowed values: "double", "float", "unsigned int", "unsigned char". Default: "double"
}

\item{spacing}{
\code{numeric vector} of length equal to the dimensions of the 'object' providing the 'spacing' information for the image to be constructed. Default: vector of 1s
}

\item{origin}{
\code{numeric vector} of length equal to the dimensions of the 'object' providing the 'origin' information for the image to be constructed. Default: vector of 0s
}
}

\value{
Object of class \code{antsImage} -- Success ; NA -- Failure
}

\author{
Shrinidhi KL
}

\examples{
\dontrun{
# create an image from a numeric array with details as shown ; here the image pixeltype is assumed to be 'double'
arr = array( c(1:1000) , dim = c(10,10,10) )
img = as.antsImage( arr , spacing = c( 1.2 , 1.2 , 2 ) , origin = c( 0 , 0 , 0 ) )
}
}
