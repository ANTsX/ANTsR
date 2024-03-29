% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getMultivariateTemplateCoordinates.R
\name{getMultivariateTemplateCoordinates}
\alias{getMultivariateTemplateCoordinates}
\title{Label multivariate components by an anatomical coordinate system.}
\usage{
getMultivariateTemplateCoordinates(
  imageSetToBeLabeledIn,
  templateWithLabels,
  labelnames = NULL,
  outprefix = NA,
  convertToTal = FALSE,
  pvals = NA,
  threshparam = 1,
  clustparam = 250,
  identifier
)
}
\arguments{
\item{imageSetToBeLabeledIn}{a template paired with (most likely) the output
of a multivariate sparse decomposition or (alternatively) could be just a
statistical map with zeroes in non-interesting areas}

\item{templateWithLabels}{e..g the mni image and brodmann label set}

\item{labelnames}{a list of names for the labels}

\item{outprefix}{if output to a file, provide file prefix}

\item{convertToTal}{bool, return talairach coordinates}

\item{pvals}{the already computed pvalue for each component}

\item{threshparam}{for pvals}

\item{clustparam}{for clusters}

\item{identifier}{unique ID for this study}
}
\value{
The output point coordinates are in approximate Talairach / MNI (or
whatever) template space.
}
\description{
This function will provide a mapping that labels the list of input images
and each of their blobs.
}
\details{
Uses getTemplateCoordinates as a sub-routine.

TBN
}
\examples{
\dontrun{
tem <- antsImageRead(getANTsRData("ch2"))
temlab <- antsImageRead(getANTsRData("ch2b"))
temlab2 <- antsImageRead(getANTsRData("ch2a"))
# try getANTsRData if you have www access
mymni <- list(
  antsImageRead(getANTsRData("mni"), 3),
  antsImageRead(getANTsRData("mnib"), 3),
  antsImageRead(getANTsRData("mnia"), 3)
)
mytem <- list(smoothImage(tem, 3), temlab, temlab2)
# mynetworkdescriptor<-getMultivariateTemplateCoordinates(
#  mytem, mymni , convertToTal = TRUE , pvals=c(0.01,0.05) )
}

}
\author{
Avants, BB
}
\keyword{Coordinates}
\keyword{Talairach,}
\keyword{Template,}
