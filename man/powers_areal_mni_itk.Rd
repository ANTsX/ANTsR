\name{powers_areal_mni_itk}
\alias{powers_areal_mni_itk}
\docType{data}
\title{powers_areal_mni_itk}
\description{
A data frame providing coordinates for the nodes identified by Powers at al.
Coordinates have been modified from nifti space to ITK space
}
\usage{data(aal)}
\format{
  A data frame listing the following variables.
  x;y;z;ROI;SystemLabel;SystemName;Color;r;g;b;Anatomy;Lobe
  \describe{
    \item{\code{x}}{ x coordinate }
    \item{\code{y}}{ y coordinate }
    \item{\code{z}}{ z coordinate }
    \item{\code{ROI}}{ unique integer for each node }
    \item{\code{SystemLabel}}{ unique integer for each system }
    \item{\code{SystemName}}{ unique name for each system }
    \item{\code{Color}}{ name of color for system }
    \item{\code{r}}{ red value for system }
    \item{\code{g}}{ green value for system }
    \item{\code{b}}{ blue value for system }
    \item{\code{Anatomy}}{ anatomical location as determined by OASIS labeling }
    \item{\code{Lobe}}{ lobe location of node }
  }
}
\references{
\url{http://www.nil.wustl.edu/labs/petersen/Resources.html}
}
\examples{
data(powers_areal_mni_itk)
}
\keyword{datasets}
