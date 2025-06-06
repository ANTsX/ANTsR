% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/labelImageRegistration.R
\name{labelImageRegistration}
\alias{labelImageRegistration}
\title{Perform label image registration.}
\usage{
labelImageRegistration(
  fixedLabelImages,
  movingLabelImages,
  fixedIntensityImages = NULL,
  movingIntensityImages = NULL,
  fixedMask = NULL,
  movingMask = NULL,
  initialTransforms = "affine",
  typeOfDeformableTransform = "antsRegistrationSyNQuick[so]",
  labelImageWeighting = 1,
  outputPrefix = "",
  randomSeed = NULL,
  verbose = FALSE
)
}
\arguments{
\item{fixedLabelImages}{A single (or set of) fixed label image(s).}

\item{movingLabelImages}{A single (or set of) moving label image(s).}

\item{fixedIntensityImages}{Optional---a single (or set of) fixed 
intensity image(s).}

\item{movingIntensityImages}{Optional---a single (or set of) moving 
intensity image(s).}

\item{fixedMask}{Defines region for similarity metric calculation 
in the space of the fixed image.}

\item{movingMask}{Defines region for similarity metric calculation 
in the space of the moving image.}

\item{initialTransforms}{If specified, there are two options:
1) Use label images with the centers of mass to a calculate linear 
transform of type  \code{'identity'}, \code{'rigid'}, 
\code{'similarity'}, \code{'affine'}.  2) Specify a list of transform
files, e.g., the output of \code{antsRegistration}.}

\item{typeOfDeformableTransform}{Only works with deformable-only transforms, 
specifically the family of \code{antsRegistrationSyN*[so]} or 
\code{antsRegistrationSyN*[bo]} transforms.  See 'typeOfTransform' 
in \code{antsRegistration}.  Additionally, one can use a list
to pass a more tailored deformable-only transform optimization using 
SyN or BSplineSyN transforms.  The order of parameters in the list
would be 1) transform specification, i.e. "SyN" or "BSplineSyN", 
2) gradient (real), 3) intensity metric (string), 4) intensity metric 
parameter (real), 5) convergence iterations per level (vector) 6) 
smoothing factors per level (tuple), 7) shrink factors per level 
(vector).  An example would typeOfDeformableTransform = list("SyN", 0.2, 
"CC", 4, c(100,50,10), c(2,1,0), c(4,2,1)).}

\item{labelImageWeighting}{Float or vector of floats giving the relative 
weighting for the label images.}

\item{outputPrefix}{String definining the output prefix for the filenames
of the output transform files.}

\item{randomSeed}{Definition for deformable registration.}

\item{verbose}{Print progress to the screen.}
}
\value{
outputs a list containing:
\describe{
  \item{fwdtransforms: }{Transforms to move from moving to fixed image.}
  \item{invtransforms: }{Transforms to move from fixed to moving image.}
}
Output of 1 indicates failure.
}
\description{
Perform pairwise registration using fixed and moving sets of label 
images (and, optionally, sets of corresponding intensity images).
}
\examples{
\dontrun{
r16 <- antsImageRead( getANTsRData( "r16" ) )
r16Seg1 <- thresholdImage( r16, "Kmeans", 3 ) - 1
r16Seg2 <- thresholdImage( r16, "Kmeans", 5 ) - 1
r64 <- antsImageRead( getANTsRData( "r64" ) )
r64Seg1 <- thresholdImage( r64, "Kmeans", 3 ) - 1
r64Seg2 <- thresholdImage( r64, "Kmeans", 5 ) - 1
reg <- labelImageRegistration( list( r16Seg1, r16Seg2 ),
                                     list( r64Seg1, r64Seg2 ),  
                                     fixedIntensityImages = r16,
                                     movingIntensityImages = r64,
                                     initialTransforms = 'affine',
                                     typeOfDeformableTransform = 'antsRegistrationSyNQuick[bo]',
                                     labelImageWeighting = c( 1.0, 2.0 ),
                                     verbose = TRUE )
}

}
\author{
Tustison NJ
}
