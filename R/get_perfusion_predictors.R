get_perfusion_predictors <- function( mat , motionparams , xideal = NULL , labelfirst = 1 , ncompcorparameters = 3 )
{
if( is.null( xideal ) )
{
  if( !labelfirst )
  {
  xideal<-( rep(c(1,0),dim(mat)[1])[1:dim(mat)[1]]-0.5 ) # control minus tag
  }else
  {
  xideal<-( rep(c(0,1),dim(mat)[1])[1:dim(mat)[1]]-0.5 ) # tag minus control
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
globalsignal<-residuals( lm( rowMeans(mat) ~ xideal ) )

# here is a 2nd (new) way to deal with motion nuisance vars - svd - just keep top 3 components
msvd<-svd( t( motionnuis[ 2:nrow( motionnuis ) ,  ] ) )
nsvdcomp<-3
motionnuis<-( msvd$u[,1:nsvdcomp] )
print( paste( ' % var of motion ' , (  sum( msvd$d[1:nsvdcomp] )/  sum( msvd$d ) ) ) )
motionnuis<-t(motionnuis)
motnames<-paste("motion",c(1:nrow(motionnuis)),sep='')
nuis<-t( rbind(globalsignal, metricnuis, (motionnuis) )  )
colnames(nuis)<-c("globalsignal","metricnuis",motnames)
# compute temporal variance of each column and apply CompCor
temporalvar<-apply(mat, 2, var)
tvhist<-hist(temporalvar , breaks = 100, plot=T)
percvar<-0.03 # percentage of high variance data to use
thresh<-tvhist$mids[  cumsum( rev( tvhist$counts / sum(tvhist$counts) > percvar ) ) == T ]
wh<-( temporalvar > thresh )
highvarmat<-mat[,wh]
compcorrsvd<-svd( highvarmat %*% t( highvarmat ) )
# ncompcorparameters<-4
if( ncompcorparameters > 0 )
  {
  compcorr<-t( compcorrsvd$u[1:ncompcorparameters, ] )
  compcorrnames<-paste("compcorr",c(1:ncol(compcorr)),sep='')
  nuis<-cbind(nuis,compcorr)
  colnames(nuis)<-c("globalsignal","metricnuis",motnames,compcorrnames)
  }
return( list( xideal = xideal , nuis = nuis ) )
}
