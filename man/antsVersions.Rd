\name{antsVersions}
\alias{antsVersions}
\docType{data}
\title{antsVersions}
\description{
A data frame defining the git tag for the current ANTsRCore version of ANTs.
}
\usage{data(antsVersions)}
\format{
  A data frame listing the following variables.
  \describe{
    \item{\code{Dependency}}{ Name of software dependency. }
    \item{\code{GitTag}}{ The git tag.  This can also be used to trace other dependencies, e.g. the ITK version used by the current ANTs version. }
  }
}
\references{
\url{https://github.com/stnava/ANTs}
}
\examples{
data(antsVersions)
}
\keyword{datasets}
