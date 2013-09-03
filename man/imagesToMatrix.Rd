\name{imagesToMatrix}
\alias{imagesToMatrix}

\title{
Read Images into a Matrix
}

\description{
  Read images into rows of a matrix.
}

\usage{
imagesToMatrix(imageList, mask)
}

\arguments{
  \item{imageList}{
    A character vector containing a list of image files to read, in
    order.
  }
  \item{mask}{
    An \code{antsImage} containing a binary mask, voxels in the mask are
    placed in the matrix.
  }
}


\value{
  A matrix containing the masked data, the result of calling
  \code{as.numeric(image, mask)} on each input image.
}
\seealso{
  \code{\link{matrixToImages}, \link{getMask}}
} 

\author{
  Cook PA
}

\examples{
\dontrun{

  imageList = list.files("/mnt/data/brains", pattern = "\\.nii\\.gz$", full.names = TRUE)

  mask = getMask("/mnt/data/masks/brainmask.nii.gz")

  mat = imagesToMatrix(imageList, mask)

}

}
