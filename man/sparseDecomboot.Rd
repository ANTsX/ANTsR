\name{sparseDecomboot}
\alias{sparseDecomboot}

\title{
Convenience wrapper for eigenanatomy decomposition.
}

\description{
Decomposes a matrix into sparse eigenevectors to maximize explained variance.
}

\usage{
sparseDecomboot( inmatrix=NA,  inmask=NA , sparseness=0.01 , nvecs=50 , its=5 , cthresh=250  , nboot = 20, nsamp = 0.9 )
}

\arguments{
\item{inmatrix}{
  n by p input images , subjects or time points by row , spatial variable lies along columns
}

\item{inmask}{
optional antsImage mask
}

\item{nboot}{
n bootstrap runs 
}

\item{nsamp}{
number of samples e.g. 0.9 indicates 90 percent of data 
}

\item{other params}{
see sccan for other parameters
}

}

\value{
outputs a decomposition of a population or time series matrix
}

\author{
Avants BB
}

\examples{
\dontrun{
mat<-replicate(100, rnorm(20)) 
mydecom<-sparseDecomboot( mat ) 
# for prediction
library(spls)
library(randomForest)
data(lymphoma)
training<-sample( rep(c(TRUE,FALSE),31)  )
sp<-0.001 ; myz<-0 ; nv<-5
ldd<-sparseDecomboot( lymphoma$x[training,], nvecs=nv , sparseness=( sp ), mycoption=1, z=myz , nsamp=0.9, nboot=50 ) # NMF style
outmat<-as.matrix(ldd$eigenanatomyimages )
# outmat<-t(ldd$cca1outAuto)
traindf<-data.frame( lclass=as.factor(lymphoma$y[ training  ]), eig = lymphoma$x[training,]  %*% outmat )
testdf<-data.frame(  lclass=as.factor(lymphoma$y[ !training ]), eig = lymphoma$x[!training,] %*% outmat )
myrf<-randomForest( lclass ~ . ,   data=traindf )
predlymp<-predict(myrf, newdata=testdf)
print(paste("N-errors:",sum(abs( testdf$lclass != predlymp ) )," non-zero ",sum(abs( outmat ) > 0 ) ) )
for ( i in 1:nv )
  print(paste(" non-zero ",i,' is: ',sum(abs( outmat[,i] ) > 0 ) ) )
}
}
