\name{sparseDecom}
\alias{sparseDecom}

\title{
Convenience wrapper for eigenanatomy decomposition.
}

\description{
Decomposes a matrix into sparse eigenevectors to maximize explained variance.
}

\usage{
sparseDecom( inmatrix=NA,  inmask=NA , sparseness=0.01 , nvecs=50 , its=5 , cthresh=250 )
}

\arguments{
\item{inmatrix}{
  n by p input images , subjects or time points by row , spatial variable lies along columns
}

\item{inmask}{
optional antsImage mask
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
mydecom<-sparseDecom( mat ) 
# for prediction
library(spls)
data(lymphoma)
training<-sample( rep(c(TRUE,FALSE),31)  )
ldd<-sparseDecom( lymphoma$x[training,], nvecs=5 , sparseness=( 0.005 ), mycoption=1, z=0.5 ) # NMF style
traindf<-data.frame( lclass=as.factor(lymphoma$y[ training  ]), eig = lymphoma$x[training,]  %*% as.matrix(ldd$eigenanatomyimages ))
testdf<-data.frame(  lclass=as.factor(lymphoma$y[ !training ]), eig = lymphoma$x[!training,] %*% as.matrix(ldd$eigenanatomyimages ))
myrf<-randomForest( lclass ~ . ,   data=traindf )
predlymp<-predict(myrf, newdata=testdf)
print(paste("N-errors:",sum(abs( testdf$lclass[i] != predlymp[i] ) )," non-zero ",sum(abs( ldd$eigenanatomyimages ) > 0 ) ) )
# compare to http://arxiv.org/pdf/0707.0701v2.pdf 
}
}
