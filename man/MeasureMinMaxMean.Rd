\name{MeasureMinMaxMean}
\alias{MeasureMinMaxMean}
\title{
Measure Min Max Mean
}
\description{
Measure min, max, and mean of image.
}
\usage{
MeasureMinMaxMean(image)
}
\arguments{
  \item{image}{Input image of type \code{antsImage}.}
}

\value{
List with entries \code{img.min}, \code{img.max}, \code{img.mean}.
}

\author{ Kandel BM.}
\examples{
myimg <- as.antsImage(array(rnorm(1000), dim=c(10, 10, 10)))
MeasureMinMaxMean(myimg)
}

