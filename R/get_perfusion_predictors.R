get_perfusion_predictors <- function( mat , motionparams , xideal = NULL , labelfirst = 1 , ncompcorparameters = 3 )
{
if( is.null( xideal ) )
{
  if( !labelfirst )
  {
  xideal<-( rep(c(1,0),dim(mat)[1])[1:dim(mat)[1]]-0.5 ) # control minus tag
  }else
  {
  xideal<-( rep(c(0,1),dim(mat)[1])[1:dim(mat)[1]]-0.5 ) # control minus tag
  }
}else if( length( xideal ) != dim(mat)[1] )
{
  print( "'xideal' must have length equal to dim(mat)[1]" )
  return( NULL )
}
# get nuisance variables : motion, compcor, etc
# motionparams <- as.data.frame( moco_params ) 
motionnuis<-t(motionparams)[2:ncol( motionparams ) , ] # matrix elements
metricnuis<-motionnuis[1,]
matrixnuis<-sqrt( motionnuis[2,]*motionnuis[2,] +  motionnuis[3,]*motionnuis[3,] + motionnuis[4,]*motionnuis[4,] + motionnuis[5,]*motionnuis[5,] +  motionnuis[6,]*motionnuis[6,] + motionnuis[7,]*motionnuis[7,] + motionnuis[8,]*motionnuis[8,] +  motionnuis[9,]*motionnuis[9,] + motionnuis[10,]*motionnuis[10,] )
matrixnuis<-sqrt(  motionnuis[3,]*motionnuis[3,] + motionnuis[4,]*motionnuis[4,] + motionnuis[5,]*motionnuis[5,] + motionnuis[7,]*motionnuis[7,] + motionnuis[8,]*motionnuis[8,] +  motionnuis[9,]*motionnuis[9,]  )
transnuis<-sqrt( motionnuis[11,]*motionnuis[11,] +  motionnuis[12,]*motionnuis[12,] + motionnuis[13,]*motionnuis[13,]  )
globalsignal<-residuals( lm( rowMeans(mat) ~ xideal ) )
nuis<-t( rbind(globalsignal, metricnuis, transnuis , matrixnuis )  )

# compute temporal variance of each column and apply CompCor
temporalvar<-apply(mat, 2, var)
tvhist<-hist(temporalvar , breaks = 20, plot=FALSE)
percvar<-0.015 # percentage of high variance data to use
thresh<-tvhist$mids[  cumsum( rev( tvhist$counts / sum(tvhist$counts) > percvar ) ) == T ]
wh<-( temporalvar > thresh )
highvarmat<-mat[,wh]
compcorrsvd<-svd( highvarmat %*% t( highvarmat ) )
# ncompcorparameters<-4
if( ncompcorparameters > 0 )
  {
  compcorr<-t( compcorrsvd$u[1:ncompcorparameters, ] )
  nuis<-cbind(nuis,compcorr)
  }

return( list( xideal = xideal , nuis = nuis ) )
}
