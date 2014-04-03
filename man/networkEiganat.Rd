\name{networkEiganat}
\alias{networkEiganat}

\title{
Convenience wrapper for eigenanatomy decomposition.
}

\description{
Decomposes a matrix into sparse eigenevectors to maximize explained variance.
}

\usage{
networkEiganat( Xin, sparam = c(0.1, 0.1), k = 5, its = 100, gradparam = 1, mask = NA, v, prior, pgradparam = 0.01)
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
mydecom<-networkEiganat( mat, nvecs=5 ) 
library(randomForest)
library(BGLR)
data(mice)
snps<-quantifySNPs( mice.X )
numericalpheno<-as.matrix( mice.pheno[,c(4,5,13,15) ] )
numericalpheno<-residuals( lm( numericalpheno ~ as.factor(mice.pheno$Litter) ) )
phind<-3
nfolds<-6
train<-sample( rep( c(1:nfolds), 1800/nfolds ) )
train<-( train < 4 )
lowr<-lowrankRowMatrix(as.matrix( snps[train,] ),900)
snpdS<-sparseDecom( lowr , nvecs=20 , sparseness=( -0.001) )
snpdF<-sparseDecom( lowrankRowMatrix(as.matrix( snps[train,] ),100) , nvecs=20 , sparseness=( -0.001) )
projmat<-as.matrix( snpdS$eig ) 
projmat<-as.matrix( snpdF$eig ) 
snpdFast<-networkEiganat( as.matrix( snps[train,] ), nvecs=20 , sparseness=c( 1, -0.001 ) , downsample=45, verbose=T, its=20, gradparam=10 )
snpdSlow<-networkEiganat( as.matrix( snps[train,] ), nvecs=20 , sparseness=c( 1, -0.001 ) , downsample=0, verbose=T, its=20, gradparam=10 )
snpd<-snpdSlow
snpd<-snpdFast
projmat<-as.matrix( snpd$v ) 
snpdF<-sparseDecom( lowrankRowMatrix(as.matrix( snps[train,] ),10) , nvecs=20 , sparseness=( -0.001) )
projmat<-as.matrix( snpdS$eig ) 
snpse<-as.matrix( snps[train, ]  ) %*% projmat 
traindf<-data.frame( bmi=numericalpheno[train,phind] , snpse=snpse)
snpse<-as.matrix( snps[!train, ]  ) %*% projmat 
testdf <-data.frame( bmi=numericalpheno[!train,phind] , snpse=snpse )
myrf<-glm( bmi ~ . , data=traindf )
preddf<-predict(myrf, newdata=testdf )
cor.test(preddf, testdf$bmi )
library(visreg)
mydf<-data.frame( PredictedBMIfromSNPs=preddf, RealBMI=testdf$bmi )
mymdl<-lm( PredictedBMIfromSNPs ~ RealBMI, data=mydf) 
visreg(mymdl)
###########
# vs glmnet #
###########
library(glmnet)
kk<-glmnet(y=numericalpheno[train,phind],x=snps[train,] )
ff<-predict(kk,newx=snps[!train,])
cor.test(ff[,25],numericalpheno[!train,phind])
mydf<-data.frame( PredictedBMIfromSNPs=ff[,25], RealBMI=testdf$bmi )
mymdl<-lm( PredictedBMIfromSNPs ~ RealBMI, data=mydf) 
visreg(mymdl)
###########
}
}
