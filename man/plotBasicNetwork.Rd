\name{plotBasicNetwork}
\alias{plotBasicNetwork}

\title{
Simple plotBasicNetwork function.
}

\description{ 
  takes an object output from renderSurfaceFunction and a list of centroids and plots the centroid network over the rendering object
}

\usage{
plotBasicNetwork( centroids = centroids, brain )
}

\arguments{
\item{centroids}{
  input matrix of size number of 3D points ( in rows ) by 3 ( in columns ) 
}
\item{brain}{
  input rendering object which is output of renderSurfaceFunction or a function derived from renderSurfaceFunction 
}
}

\value{
None
}

\author{
Avants BB and Duda JT
}

\examples{
\dontrun{
brain<-renderSurfaceFunction( surfimg =list( mymni[[1]] ) , alphasurf=0.1 , smoothsval = 1.5  )
plotBasicNetwork( centroids = centroids, brain )
}
}
