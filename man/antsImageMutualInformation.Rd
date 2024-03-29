% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/antsImageMutualInformation.R
\name{antsImageMutualInformation}
\alias{antsImageMutualInformation}
\title{mutual information between two images}
\usage{
antsImageMutualInformation(in_image1, in_image2, sampling.percentage, nBins)
}
\arguments{
\item{in_image1}{antsImage}

\item{in_image2}{antsImage}

\item{sampling.percentage}{value between zero and one determining ratio of points in image to
use for metric estimation}

\item{nBins}{number of bins to use in joint histogram estimate, often set between 8 and 32}
}
\value{
mutual information value
}
\description{
compute mutual information between two images
}
\examples{

fi <- antsImageRead(getANTsRData("r16"), 2)
mi <- antsImageRead(getANTsRData("r64"), 2)
testthat::expect_error(antsImageMutualInformation(fi, "hey"))
testthat::expect_error(antsImageMutualInformation("hey", fi))
fi3 <- resampleImage(fi, c(2, 2))
fi2 <- antsImageClone(fi)
pixeltype(fi2) <- "unsigned int"
testthat::expect_error(antsImageMutualInformation(fi2, mi))
testthat::expect_error(antsImageMutualInformation(fi3, mi))

}
\author{
Brian B. Avants
}
