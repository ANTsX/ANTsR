\name{initializeEigenanatomy}
\alias{initializeEigenanatomy}

\title{
Convert a matrix to a form that can be used to initialize sparse cca and pca.
}

\description{
InitializeEigenanatomy is a helper function to initialize sparseDecom and sparseDecom2.
}

\usage{
initdataframe<-initializeEigenanatomy(  initmat  , mask=NA, nreps=1  )
}

\arguments{
\item{mat}{
  input matrix where rows provide initial vector values
}
\item{nreps}{
  nrepetitions to use 
}
\item{mask}{
  mask if available
}
}

\value{
list is output
}

\author{
Avants BB
}

\examples{
mat <- matrix(c(rep(1,10),rep(0,20)),ncol=10)
initdf<-initializeEigenanatomy( mat ) 
initdf2<-initializeEigenanatomy( mat, nreps=2 ) 
}
