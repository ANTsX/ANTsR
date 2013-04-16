networkEiganat<-function( Xin , sparam=c(0.1,0.1), k = 5 , its = 100 , gradparam = 1 , mask = NA , v, prior, pgradparam = 1.e-2  ) 
{
X<-Xin-min(Xin)
print(paste("Implements: ||  X - U V ||  +   || XP -  XV ||^2 + ell1( V )=",sparam))
############################
# gradient 1               #
#   U^T ( X - U V^T )      #
#       ( X - U V^T ) V    #
# gradient 2               #
#   X^T ( X * ( P - V ) )  #
############################
if ( missing( v ) )
  {
  v<- t( whiten(replicate( ncol(X) , rnorm(k)) ) )
  }
# v<-sparsifyv( v, sparam[2], mask )
u <- ( X %*% v ) 
for ( jj in 1:its ) {
  for ( a in 1:nrow(X) )
    {
    tt<-c(u[a, ])
    if ( jj == 1 ) tt<-c(u[a, ] *1.e-9)
    usol<-conjGradS( A=v , x_k=tt , b_in=c(X[a,]), sp=sparam[1] )
    u[a, ]<-usol 
    }
  v <- v + t( t( u ) %*% ( X  - u %*% t(v)  ) ) * gradparam
  if ( ! missing( prior )  )
    {
    v <- v + t(X) %*% ( X %*% ( prior - v ) ) * pgradparam
    }
  v<-sparsifyv( v, sparam[2], mask )
  if (  missing(prior) ) print( paste("Data",norm(X - u %*% t(v), "F" ) ) )
  if ( ! missing(prior) ) print( paste("Data",norm(X - u %*% t(v), "F" ), "Prior",norm( prior - v, "F") ))
}
for ( a in 1:nrow(X) )
  {
  usol<-conjGradS( A=v , x_k=c(u[a, ]) , b_in=c(X[a,]), sp=sparam[1] )
  u[a, ]<-usol 
  }
return( list( u=t(u), v=t(v) ) )
}

sparsifyv<-function( v, sparam, mask = NA )
  {
  if ( sparam >= 1 ) return( v )
  b<-round(  abs( as.numeric(sparam) ) * nrow(v) )
  if ( b < 1 ) b <-1
  if ( b > nrow(v) ) b<-nrow(v)
  for ( i in 1:ncol(v) )
    {
    sparsev<-(v[,i])
    if ( sparam > 0 ) sparsev<-( sparsev - min( sparsev ) ) 
    ord<-rev( order( sparsev ) )
    sparsev[ ord[(b):length(ord) ]]<-0
    if ( ! is.na( mask ) ) {
      vecimg<-antsImageClone( mask )
      vecimg[ mask > 0 ]<-sparsev
   #   ImageMath( mask@dimension, vecimg,"ClusterThresholdVariate",vecimg,100)
    }
    v[,i] <- sparsev
    }
  return(v)
  }

