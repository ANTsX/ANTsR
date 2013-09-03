\name{visualizeBlob}
\alias{visualizeBlob}

\title{
Visualize cortical blob
}

\description{
Visualize a cortical blob on a white matter segmentation of a template.
}

\usage{
visualizeBlob(template, blob, outname='wmBlob', dim=3)
}

\arguments{
  \item{template}{Template used for visualization.  Can be image or file name to be read}
  \item{blob}{Blob to be visualized.  Assumed to be in same space as \code{template}.  Can also be image or file.}
  \item{outname}{Prefix for output png images.}
  \item{dim}{Dimension of images.}
}
\details{
  Visualization settings are optimized for 1mm isotropic resolution images.  Viewing angles may also need to be modified for different templates.
}
\value{
  List with entries \code{mask} (the mask for the template), 
  \code{seg} (the 3-tissue segmentation of the template), 
  and \code{glassbrain} (the glass brain generated from the template).
 }

\author{Kandel BM, Avants BB
}

\examples{
template <- antsImageRead(getANTsRData("mni"), 3)
lab <- antsImageRead(getANTsRData("mnia"), 3)
blob <- maskImage(lab, lab, 21)
visualizeBlob(template, blob)
}
