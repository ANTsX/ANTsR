% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multiLabelMorphology.R
\name{multiLabelMorphology}
\alias{multiLabelMorphology}
\title{Morphology on multi label images.}
\usage{
multiLabelMorphology(
  labelsIn,
  operation,
  radius,
  dilationMask = NULL,
  labelList = NA
)
}
\arguments{
\item{labelsIn}{Input image should contain only 0 for background and positive integers for
labels.}

\item{operation}{One of \code{'MD'}, \code{'ME'}, \code{'MC'}, \code{'MO'}, passed to iMath.}

\item{radius}{radius of the morphological operation.}

\item{dilationMask}{Optional binary mask to constrain dilation only (eg dilate cortical
label into WM).}

\item{labelList}{Optional list of labels, to perform operation upon. Defaults to all unique
intensities in \code{labels.in}.}
}
\value{
processed labels, of type antsImage.
}
\description{
Wraps calls to iMath binary morphology. Additionally, dilation and closing operations preserve
pre-existing labels. The choices of operation are:
}
\details{
Dilation: dilates all labels sequentially, but does not overwrite original labels.
This reduces dependence on the intensity ordering of adjoining labels. Ordering dependence
can still arise if two or more labels dilate into the same space - in this case, the label
with the lowest intensity is retained. With a mask, dilated labels are multiplied by the
mask and then added to the original label, thus restricting dilation to the mask region.

Erosion: Erodes labels independently, equivalent to calling iMath iteratively.

Closing: Close holes in each label sequentially, but does not overwrite original labels.
Opening: Opens each label independently, equivalent to calling iMath iteratively.
}
\examples{

img <- antsImageRead(getANTsRData("r16"), 2)
labels <- getMask(img, 1, 150) + getMask(img, 151, 225) * 2
labelsDilated <- multiLabelMorphology(labels, "MD", 2)

# should see original label regions preserved in dilated version
# label N should have mean N and 0 variance
labelStats(labelsDilated, labels)
}
\author{
Cook PA
}
