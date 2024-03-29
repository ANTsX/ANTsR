% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zzz_Summary.R
\name{sd}
\alias{sd}
\alias{sd.default}
\alias{sd.antsImage}
\title{SD generic}
\usage{
sd(x, na.rm = FALSE, ...)

\method{sd}{default}(x, na.rm = FALSE, ...)

\method{sd}{antsImage}(x, na.rm = FALSE, ..., mask = NULL)
}
\arguments{
\item{x}{an object for which we want to compute the SD}

\item{na.rm}{a logical value indicating whether NA should be removed}

\item{\dots}{Any additional arguments to be passed to
\code{\link[stats]{sd}}}

\item{mask}{is an object of class \code{antsImage}}
}
\description{
Calculates the SD of an image

Overloaded SD for antsImage objects
}
\examples{
img <- antsImageRead(getANTsRData("r16"))
sd(img)
sd(img, mask = img > 0)
}
