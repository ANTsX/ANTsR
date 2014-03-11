\name{icawhiten}
\alias{icawhiten}

\title{
Simple icawhitening function.
}

\description{
Icawhitens the input matrix using SVD and returns the result.
}

\usage{
wmat<-icawhiten( mat , n.comp=2 )
}

\arguments{
\item{mat}{
  input matrix
}
\item{n.comp}{
  number of components on which to project
}
}

\value{
matrix is output
}

\author{
Avants BB, fastICA package (see CRAN)
}

\examples{
mat <- matrix(c(rep(1,100),rep(0,200)),ncol=50)
wmat<-icawhiten( mat, 2 ) 
}
