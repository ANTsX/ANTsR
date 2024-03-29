% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hist.antsImage.R
\name{hist.antsImage}
\alias{hist.antsImage}
\title{Histogram of Values in an Image}
\usage{
\method{hist}{antsImage}(x, ..., mask)
}
\arguments{
\item{x}{Object of class \code{antsImage}}

\item{...}{Arguments passed to \code{\link{hist.default}}}

\item{mask}{object to subset the image.  If missing, then all
values of the image are plotted.}
}
\value{
Output of \code{\link{hist}}
}
\description{
Computes and displays a histogram of the values
of an image with the option for a mask.
}
\examples{
img <- makeImage(c(10, 10), rnorm(100))
mask <- img > 0
hist(img)
hist(img, mask = mask)
}
