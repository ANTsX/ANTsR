conjGradS<-function( A_in = NA, x_k= NA, b_in = NA, maxits=100 , sp = -1 , convcrit = 1.e-5 )
{
  # This will use the normal equations */
  # Based on Golub CONJUGATE  G R A D I E N T   A N D  LANCZOS  HISTORY
  # http://www.matematicas.unam.mx/gfgf/cg2010/HISTORY-conjugategradient.pdf
  # minimize the following error :    \| A^T*A * vec_i -    b \|  +  sparseness_penalty
  A<-A_in # decostand( A_in , method="standardize",MARGIN=2)
  At <-t( A )
  b <- b_in - mean( b_in )
  b <- b %*% A;
  r_k <- At %*% ( A %*% as.matrix( x_k ) );
  r_k <- c(b) - c(r_k)
  p_k <- r_k;
  approxerr <- 1.e9;
  ct <- 0;
  bestsol <- x_k;
  starterr <- norm( b );
  minerr <- starterr; deltaminerr <- 1; lasterr <- starterr * 2;
  while(  deltaminerr > 0 & approxerr > convcrit & ct < maxits )
    {
#    print(paste("ct",ct,length(p_k),deltaminerr,approxerr))
    temp<- ( A %*%  c(p_k) )  
    temp<- c( At %*% temp )
    alpha_denom <- sum(  p_k * temp )
    iprk <- sum( r_k  *  r_k );
    if( alpha_denom < 1.e-12 )
      {
      alpha_denom <- 1;
      }
    alpha_k <- iprk / alpha_denom;
    x_k1  <-c( x_k ) + c( p_k ) * alpha_k; # this adds the scaled residual to the current solution
    x_k1 <- sparsifyv( as.matrix(x_k1) , sp);  # this can be used to approximate NMF
    temp<-At  %*% (A %*% as.matrix(x_k1) )
    r_k1 <-  b - c( temp )
    approxerr <- norm( r_k1 )
    if( approxerr < minerr )
      {
      minerr <- approxerr; bestsol <- ( x_k1 );
      }
    deltaminerr <- ( lasterr - approxerr );
    lasterr <- approxerr;
    # measures the change in the residual --- this is the Fletcher-Reeves form for nonlinear CG
    # see Table 1.1 \Beta^FR in A SURVEY OF NONLINEAR CONJUGATE GRADIENT METHODS
    # in this paper's notation d => p,  g => r , alpha, beta, x the same , so y = rk1 - rk
    # measures the change in the residual
    yk <- r_k1 - r_k;
    bknd <-  sum( p_k * yk );
    beta_k <- sum( r_k1 * r_k1 ) / sum( r_k * r_k ); # classic cg
    p_k1  <- r_k1 + beta_k * p_k;
    r_k <- r_k1;
    p_k <- p_k1;
    x_k <- x_k1;
    ct<-ct+1
    }
  x_k <- bestsol;
  return( x_k )
}

