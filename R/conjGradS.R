conjGradS<-function( A_in = NA, x_k= NA, b_in = NA, maxits=1000 , sp = -10 , convcrit = 0 )
{
  # This will use the normal equations */
  # Based on Golub CONJUGATE  G R A D I E N T   A N D  LANCZOS  HISTORY
  # http://www.matematicas.unam.mx/gfgf/cg2010/HISTORY-conjugategradient.pdf
  # minimize the following error :    \| A^T*A * vec_i -    b \|  +  sparseness_penalty
  A<-A_in #decostand( A_in , method="standardize",MARGIN=2)
 # A<-decostand( A_in , method="standardize",MARGIN=2)
  At <-t( A )
  b <- b_in # - mean( b_in )
  b <- b %*% A;
  r_k <- At %*% ( A %*% as.matrix( x_k ) );
  r_k <- c(b) - c(r_k)
  p_k <- r_k;
  approxerr <- 1.e9;
  ct <- 0;
  bestsol <- x_k;
  starterr <-  sum( b * b )
  minerr <- starterr; deltaminerr <- 1; lasterr <- starterr * 2;
  badct<-0
  while(  deltaminerr > 0 & approxerr > convcrit & ct < maxits )
#  while(  badct < 2 & ct < maxits )
    {
#    print(paste("ct",ct,length(p_k),deltaminerr,approxerr,' bad ', badct))
    temp<- ( A %*%  c(p_k) )  
    temp<- c( At %*% temp )
    alpha_denom <- c(  p_k %*% temp )
    iprk <- sum( r_k  *  r_k );
    alpha_k <- iprk / alpha_denom;
    x_k1  <-c( x_k ) + c( p_k ) * alpha_k ; # this adds the scaled residual to the current solution
    x_k1 <- c(sparsifyv( as.matrix(x_k1) , sp))  
    temp<-At  %*% (A %*% as.matrix(x_k1) )
    r_k1 <-  b - c( temp )
    approxerr <- norm( r_k1 )
    if( approxerr < minerr )
      {
      minerr <- approxerr; bestsol <- ( x_k1 );
      }
    deltaminerr <- ( lasterr - approxerr );
    lasterr <- approxerr;
    yk <- r_k1 - r_k;
    bknd <-  sum( p_k * yk );
    beta_k <- sum( ( yk - p_k * 2 * norm(yk) / bknd ) * ( r_k1 / bknd ) );
    p_k1  <- ( r_k1 + beta_k * p_k ) 
    r_k <- r_k1;
    p_k <- p_k1;
    x_k <- x_k1;
    ct<-ct+1
    if ( deltaminerr < 0 ) badct<-badct+1
    }
  x_k <- bestsol;
  return( x_k )
}

