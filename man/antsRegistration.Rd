\name{antsRegistration}
\alias{antsRegistration}

\title{
A simplified (or full) interface to antsRegistration.
}

\description{
Register a pair of images either through the full or simplified interface.  Uses file I/O to manage images / transformations.
}

\usage{
antsRegistration( dimension , fixed , moving , outputPrefix="./antsRegOut",typeofTransform = c("Rigid","Affine","SyN"))
}

\arguments{
\item{imageDimension 2|3|4}{
Number of dimensions of the input image
}

\item{fixed}{
fixed image to which we register the moving image.
}

\item{movingImage}{
moving image to be mapped to fixed space.
}

\item{outPrefix}{
output will be named with this prefix.
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
# will give the full form of help
antsRegistration( "-h")
}
