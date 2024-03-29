% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vwnrfs.R
\name{splitMask}
\alias{splitMask}
\title{split a mask into n labeled sub-masks}
\usage{
splitMask(mask, n = NA, voxchunk = NA)
}
\arguments{
\item{mask}{antsImage mask}

\item{n}{number of mask chunks (if voxchunk is not set)}

\item{voxchunk}{number of voxels per chunk (if n is not set)}
}
\value{
relabeledMask
}
\description{
split a mask into n labeled sub-masks
}
\examples{
mask <- getMask(antsImageRead(getANTsRData("r16")))
smask <- splitMask(mask, 10)

}
\author{
Avants BB, Tustison NJ, Pustina D
}
