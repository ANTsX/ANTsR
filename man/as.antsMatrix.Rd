% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/antsMatrix.R
\name{as.antsMatrix}
\alias{as.antsMatrix}
\alias{as.antsMatrix,list-method}
\alias{as.antsMatrix,data.frame-method}
\alias{as.antsMatrix,matrix-method}
\title{Coerce Object to as.antsMatrix}
\usage{
as.antsMatrix(object, elementtype = "float", ...)

\S4method{as.antsMatrix}{list}(object, elementtype = "float")

\S4method{as.antsMatrix}{data.frame}(object, elementtype = "float")

\S4method{as.antsMatrix}{matrix}(object, elementtype = "float")
}
\arguments{
\item{object}{An object}

\item{elementtype}{e.g. "float" or "double"}

\item{...}{other parameters}
}
\description{
convert types to an antsMatrix
}
\examples{
as.antsMatrix(matrix(rnorm(10), nrow = 2))
mat <- matrix(rnorm(10), nrow = 2)
df <- as.data.frame(mat)
as.antsMatrix(df)
as.antsMatrix(as.list(df))
}
