peigenanat<-function( Xin , sparam=0.1, k = 5 , its = 100 , gradparam = 1 , mask = NA , v, prior, pgradparam = 1.e-2  ) 
{
X<-Xin-min(Xin)
print(paste("Implements: ||  X - U V ||  +   || XP -  XV ||^2 + ell1( V )=",sparam))
if ( missing( v ) )  v<-t( replicate( ncol(X) , rnorm(k)) )
u <- matrix( rep(0, ncol(v)*nrow(X) ), nrow=nrow(X), ncol=ncol(v) ,byrow=T )
for ( jj in 1:its ) {
for ( a in 1:nrow(X) )
  {
  usol<-coefficients(lm(   X[a,] ~  v   ))
  u[a, ]<-usol[2:(ncol(u)+1)]
  }
v <- v + t( t( u ) %*% ( X  - u %*% t(v)  ) ) * gradparam
if ( ! missing( prior )  )
  {
  v <- v + t(X) %*% ( X %*% ( prior - v ) ) * pgradparam
  }
v<-sparsifyv( v, sparam, mask )
if (  missing(prior) ) print( paste("Data",norm(X - u %*% t(v), "F" ) ) )
if ( ! missing(prior) ) print( paste("Data",norm(X - u %*% t(v), "F" ), "Prior",norm( prior - v, "F") ))
}
recon<-X
for ( a in 1:nrow(X) )
  {
  mdl<-lm(   X[a,] ~  v   )
  usol<-coefficients(mdl)
  u[a, ]<-usol[2:(ncol(u)+1)]
  recon[a,]<-predict( mdl )
  }
rr<-norm(X - recon, "F" )
matpfrobnorm<-norm( X , "F" )
varx<-( 1.0 - ( rr * rr ) / ( matpfrobnorm * matpfrobnorm ) )
print( paste( varx  ) )
return( list( u=u, v=v , recon=recon ) )
}

sparsifyv<-function( v, sparam, mask = NA )
  {
  if ( sparam >= 1 ) return( v )
  b<-round( sparam * nrow(v) )
  if ( b < 1 ) b <-1
  if ( b > ncol(v) ) b<-ncol(v)
  for ( i in 1:ncol(v) )
    {
    sparsev<-v[,i]
    sparsev<-sparsev-min(sparsev)
    ord<-rev( order( sparsev ) )
    sparsev[ ord[b:length(ord) ]]<-0
    if ( ! is.na( mask ) ) {
      vecimg<-anstImageClone( mask )
      vecimg[ mask > 0 ]<-sparsev
      ImageMath( mask@dimension, vecimg,"ClusterThresholdVariate",vecimg,100)
    }
    v[,i] <- sparsev
    }
  return(v)
  }

