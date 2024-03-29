% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hist.antsImage.R
\name{density.antsImage}
\alias{density.antsImage}
\title{Density of Values in an Image}
\usage{
\method{density}{antsImage}(x, ..., mask)
}
\arguments{
\item{x}{Object of class \code{antsImage}}

\item{...}{Arguments passed to \code{\link{density.default}}}

\item{mask}{object to subset the image.  If missing, then all
values of the image are plotted.}
}
\value{
Output of \code{\link{density}}
}
\description{
Computes the density of values
of an image with the option for a mask.
}
\examples{
img <- makeImage(c(10, 10), rnorm(100))
mask <- img > 0
density(img, mask = mask)
}
