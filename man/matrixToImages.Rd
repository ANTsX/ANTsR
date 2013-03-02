\name{matrixToImages}
\alias{matrixToImages}

\title{Convert rows of a matrix to images}

\description{
  Unmasks rows of a matrix and writes as images.
}

\usage{
matrixToImages(dataMatrix, mask, outputRoot)
}

\arguments{
  \item{dataMatrix}{
    A matrix such as that created by \code{imagesToMatrix}.
  }
  \item{mask}{
    An \code{antsImage} containing a binary mask. Rows of the matrix are
    unmasked and written as images. The mask defines the output image
    space. 
  }
  \item{outputRoot}{
    A string that is prepended to output image names. Output images will
    be numbered by row number in the input matrix, starting with 001. 
  }
  
}

\seealso{
  \code{\link{imagesToMatrix}, \link{getMask}}
} 

\author{
  Cook PA
}

\examples{
\dontrun{
  mat = matrixToImages(aMat, mask, "aMat_")
}

}
