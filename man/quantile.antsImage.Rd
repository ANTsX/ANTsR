% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hist.antsImage.R
\name{quantile.antsImage}
\alias{quantile.antsImage}
\title{Sample Quantiles}
\usage{
\method{quantile}{antsImage}(x, ..., mask)
}
\arguments{
\item{x}{Object of class \code{antsImage}}

\item{...}{Arguments passed to \code{\link{quantile}}}

\item{mask}{object to subset the image.  If missing, then all
values of the image are used}
}
\value{
Output of \code{\link{quantile}}
}
\description{
Computes sample
quantiles for an image, with the option of a mask.
}
\examples{
img <- makeImage(c(10, 10), rnorm(100))
mask <- img > 0
quantile(img, mask = mask)
}
