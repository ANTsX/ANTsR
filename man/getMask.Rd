\name{getMask}
\alias{getMask}

\title{Get Mask}

\description{
  Get a binary mask image from the given image after thresholding.
}

\usage{
getMask( img = NULL, lowThresh = 1, highThresh = Inf, cleanup = FALSE )
}

\arguments{
  \item{img}{
    Input image. Can be a filename of a 3D image or an \code{antsImage}
    of dimension 3. If \code{img} is NULL, a file chooser dialog will
    appear. 
  }
  \item{lowThresh}{
    An inclusive lower threshold for voxels to be included in the mask.
  }
  \item{highThresh}{
    An inclusive upper threshold for voxels to be included in the mask.
  }
  \item{cleanup}{
    If \code{TRUE}, morphological operations will be applied to clean up
    the mask by eroding away small or weakly-connected areas, and
    closing holes.
  }
}
\details{
  If \code{cleanup} is \code{TRUE}, the following steps are applied
  \enumerate{
    \item Erosion with radius 2 voxels
    \item Retain largest component
    \item Dilation with radius 1 voxel
    \item Morphological closing
  }
  
  These functions are available in \link{ImageMath}, see the operations
  \dQuote{ME}, \dQuote{GetLargestComponent}, \dQuote{MD}, \dQuote{FillHoles}.
}


\value{
  Object of type \code{antsImage} containing the masked image. The voxel
  intensities will be binarized, 1 for voxels in the mask and 0 outside.
}

\author{
  Shrinidhi KL, Cook PA
}

\examples{
\dontrun{
# input_img.nii is a 3D image
mask_img <- get_mask( "input_img.nii" , 500 , 1e9 )
}
}
\keyword{mask}