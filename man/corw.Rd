\name{corw}
\alias{corw}

\title{
produces a correlation matrix via weighted correlation.
}

\description{
Uses weighted regression to compute pairwise correlation matrix on input matrix - by columns.
}

\usage{
wmat<-corw( mat  )
}

\arguments{
\item{mat}{
  input matrix
}
}

\value{
matrix is output
}

\author{
Avants BB
}

\examples{
mat <- matrix(c(rep(1,100),rep(0,20)),ncol=10)
wcmat<-corw( mat , weights = rep(1,ncol(mat) ) ) 
}
