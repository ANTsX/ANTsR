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
mat<-t(replicate(3, rnorm(100)) )
for ( i in 1:nrow(mat) ) mat[i, abs(mat[i,]) < 1 ]<-0
initdf<-initializeEigenanatomy( mat ) 
dmat<-replicate(100, rnorm(20)) 
eanat<-sparseDecom( dmat, inmask=initdf$mask, 
  sparseness=0, smooth=0,
  initializationList=initdf$initlist, cthresh=0,
  nvecs=length(initdf$initlist) )
initdf2<-initializeEigenanatomy( mat, nreps=2 ) 
eanat<-sparseDecom( dmat, inmask=initdf$mask, 
  sparseness=0, smooth=0, z=-0.5,
  initializationList=initdf2$initlist, cthresh=0,
  nvecs=length(initdf2$initlist) )
# now a regression
eanatMatrix<-imageListToMatrix(  eanat$eigenanatomyimages, initdf$mask )
# "averages" loosely speaking anyway
myEigenanatomyRegionAverages<-mat %*% t( eanatMatrix )
dependentvariable<-rnorm( nrow(dmat) )
summary(lm( dependentvariable ~ myEigenanatomyRegionAverages ))

dmat2<-replicate(30, rnorm(20)) 
eanat<-sparseDecom2( list(dmat,dmat2), inmask=c(initdf$mask,NA), 
  sparseness=c(-0.1,-0.1 ), smooth=0,
  initializationList=initdf$initlist, cthresh=c(0,0),
  nvecs=length(initdf$initlist) )
}
