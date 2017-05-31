\name{antsrVersions}
\alias{antsrVersions}
\docType{data}
\title{antsrVersions}
\description{
A data frame defining the git tag for the current ANTsR version.  One can also
see antsrVersions from ANTsRCore to get the git tag for those dependencies.
}
\usage{data(antsrVersions)}
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
data(antsrVersions)
}
\keyword{datasets}
