\name{get-mask}
\alias{get_mask}
\alias{getmask}

\title{
Get Mask
}

\description{
Get a mask-image from the given image after thresholding.
}

\usage{
get_mask( img , thresh_lo , thresh_hi )
}

\arguments{
\item{img}{
Input image for motion-correction. Can be a filename of an 4D image or
an 'antsImage' of pixeltype 'float' and dimension '4'.
}
}

\value{
Success -- object of type 'antsImage' containing the mask-image\cr
Failure -- NULL
}

\author{
Shrinidhi KL
}

\examples{
\dontrun{
# input_img.nii is a 3D image
mask_img <- get_mask( "input_img.nii" , 500 , 1e9 )
}
}
