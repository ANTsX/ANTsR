\name{whiten}
\alias{whiten}

\title{
Simple whitening function.
}

\description{
Whitens the input matrix using SVD and returns the result.
}

\usage{
wmat<-whiten( mat  )
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
mat <- matrix(c(rep(1,100),rep(0,200)),ncol=50)
wmat<-whiten( mat ) 
}
