#' randomized svd function.
#'
#' uses random matrix to estimate svd results
#'
#' @param A input matrix
#' @param k rank to use
#' @param seed for testing
#' @return randomized svd is output
#' @author Avants BB
#' @references N. Halko, P.G. Martinsson, J. Tropp "Finding structure with randomness: Stochastic algorithms for constructing approximate matrix decompositions" arXiv 0909.4061
#' @keywords svd
#' @examples
#'
#' A <- matrix(rnorm(3000), ncol=50 )
#' k=10
#' dr=randsvd(A,k)$d[1:k]
#' dt=svd(A)$d[1:k]
#' cor(dr,dt)
#' @export randsvd
randsvd <- function( A, k, seed=NA ) {
if ( !is.na(seed) ) set.seed(seed)
#  Let A be a matrix to be analyzed with n rows and m columns, and r be the ranks of a truncated SVD (if you choose r = min(n, m), then this is the original SVD).

# First a random Gaussian matrix G with m rows and r columns is sampled and computes Y = At G.
# Then apply the Gram-Schmidt process to Y so that each column of Y is ortho-normalized.
G = ( replicate( k , rnorm( nrow(A) ) ) )
Y = .gramschmidt( t(A) %*% G )  # this is a orthornomal basis in P (sparse?)

# Then we compute B = AY with n rows and r columns. Although the size of Y is much smaller than that of A, Y holds the informatin of A; that is AYYt = A. Intuitively, the row information is compresed by Y and can be decompressed by Yt
B = A %*% Y    # subject space

# Similarly, we take another random Gaussian matrix P with r rows and r columns, and compute Z = BP. As in the previous case, the columns of Z are ortho-normalized by the Gram-Schmidt process. ZZtt B = B.
P = ( replicate( k , rnorm( k ) ) )  # gauss mat
Z = .gramschmidt( B %*% P )  # small basis again

#  Then compute C = Zt B.
C = t(Z) %*% B

# Finally we compute SVD of C using the traditional SVD solver, and obtain C = USVt where U and V are orthonormal matrices, and S is the diagonal matrix whose entriesa are singular values. Since a matrix C is very small, this time is negligible.
csvd = svd( C )

# Now A is decomposed as A = AYYt = BYt = ZZtBYt = ZCYt = ZUSVtYt

# Both ZU and YV are othornormal, and ZU is the left singular vectors and YV is the right singular vector. S is the diagonal matrix with singular values.
normer<-function( x ) { sum( sqrt( x*x ) ) }
zu = Z %*% csvd$u
nn=apply( zu, MARGIN=2, FUN=normer )
zu=t( t(zu)/nn )
yv = Y %*% csvd$v   # could be made sparse and plugged back in above ...
nn=apply( yv, MARGIN=2, FUN=normer )
yv=t( t(yv)/nn )
return( list(
  d=csvd$d,
  u=zu,
  v=yv
  ) )

}









#' randomized whitening function.
#'
#' uses random matrix to whiten a matrix
#'
#' @param A input matrix
#' @param k rank to use
#' @param seed for testing
#' @return whitened matrix is output
#' @author Avants BB
#' @references N. Halko, P.G. Martinsson, J. Tropp "Finding structure with randomness: Stochastic algorithms for constructing approximate matrix decompositions" arXiv 0909.4061
#' @keywords whiten
#' @examples
#'
#' A <- matrix(rnorm(3000), ncol=50 )
#' k=10
#' Aw=randwhiten(A,k)
#' # (Aw) %*% t(Aw)
#' @export randwhiten
randwhiten <- function( A, k, seed=NA ) {
  if ( !is.na(seed) ) set.seed(seed)
  X <- t(A)
  n.comp=k
  n <- nrow(X)
  p <- ncol(X)
  X <- scale(X, scale = FALSE)
  X <- t(X)
#  V <- X %*% t(X)/n
  G = replicate( k , rnorm( ncol( t(X) ) ) )
  Y = .gramschmidt( X %*% ( t(X) %*% G ) ) # keep this
  rm( G )
  B = X %*% ( t(X)  %*% Y )
  P = replicate( k , rnorm( k ) )
  Z = .gramschmidt( B %*% P ) # keep this
  C = t(Z) %*% B
  rm( B )
  rm( P )
  csvd = svd( C )
  rm( C )
  normer<-function( x ) { sum( sqrt( x*x ) ) }
  zu = Z %*% csvd$u
  nn=apply( zu, MARGIN=2, FUN=normer )
  zu=t( t(zu)/nn )
  yv = Y %*% csvd$v   # could be made sparse and plugged back in above ...
  nn=apply( yv, MARGIN=2, FUN=normer )
  yv=t( t(yv)/nn )
  s = list(
    d=csvd$d,
    u=zu,
    v=yv
    )
  D <- diag(c(1/sqrt(s$d)))
  K <- D %*% t(s$u)
  K <- matrix(K[1:n.comp, ], n.comp, p)
  xw <- K %*% X # lowrank in row
}



#' randomized cca function.
#'
#' uses random matrix to estimate cca results
#'
#' @param x input matrix
#' @param y input matrix
#' @param k rank to use
#' @param seed for testing
#' @return randomized svd of t(x) y is output
#' @author Avants BB
#' @examples
#' set.seed(13)
#' x <- matrix(rnorm(3000), nrow=50 )
#' y <- x %*% matrix( rnorm( 60*100 ), nrow=60)
#' k=10
#' dr=randcca(x,y,k,1)
#' dt=svd( t(x)%*%y )
#' cor(dr$d[1:k],dt$d[1:k])
#' tt=2
#' ct=0
#' for ( i in 1:20 )
#'   ct=ct+as.numeric( dr$d[tt] < randcca(x,y[sample(1:50),],k,1)$d[tt] )
#' if ( ct > 0 ) stop("should not happen")
#' \dontrun{
#' # produce low rank cca matrix
#' K = t(dr$u)
#' lowAr = ( K %*% t(x) ) %*% y
#' K = t(dt$u)[1:k,]
#' lowAt = ( K %*% t(x) ) %*% y
#' mydecom<-sparseDecom( lowAr )
#' }
#'
#' @export randcca
randcca <- function( x, y, k, seed ) {
  if ( !is.na(seed) ) set.seed(seed)
  xw = randwhiten( x , k )
  yw = randwhiten( y , k )
  G = replicate( k , rnorm( ncol(x) ) )
  Y = .gramschmidt( t(yw) %*% ( xw %*% G ) ) # keep this
  rm( G )
  B = t(xw) %*% ( yw  %*% Y )
  P = replicate( k , rnorm( k ) )
  Z = .gramschmidt( B %*% P ) # keep this
  C = t(Z) %*% B
  rm( B )
  rm( P )
  csvd = svd( C )
  rm( C )
  normer<-function( x ) { sum( sqrt( x*x ) ) }
  # nn=apply( scale(lowmat), MARGIN=1, FUN=normer )
  # low1=scale(lowmat)/nn
  normer<-function( x ) { sum( sqrt( x*x ) ) }
  zu = Z %*% csvd$u
  nn=apply( zu, MARGIN=2, FUN=normer )
  zu=t( t(zu)/nn )
  yv = Y %*% csvd$v   # could be made sparse and plugged back in above ...
  nn=apply( yv, MARGIN=2, FUN=normer )
  yv=t( t(yv)/nn )
  s=list(
    d=csvd$d,
    u=zu,
    v=yv
    )
  D <- diag(c(1/sqrt(s$d)))
  K <- D %*% t(s$u)
  return( ( K %*% t(xw) ) %*% yw )
}

.gramschmidt <- function( A, tol=1.e-8 ) {
  A.qr <- qr(A)
  return( qr.Q(A.qr) )
    stopifnot(is.numeric(A), is.matrix(A))
    m <- nrow(A)
    n <- ncol(A)
    if (m < n)
        stop("No. of rows of 'A' must be greater or equal no. of colums.")
    Q <- matrix(0, m, n)
    R <- matrix(0, n, n)
    for (k in 1:n) {
        Q[, k] <- A[, k]
        if (k > 1) {
            for (i in 1:(k - 1)) {
                R[i, k] <- t(Q[, i]) %*% Q[, k]
                Q[, k] <- Q[, k] - R[i, k] * Q[, i]
            }
        }
        R[k, k] <- sum(abs(Q[, k])^2)^(1/2.0) # Norm(Q[, k])
        if (abs(R[k, k]) <= tol)
            stop("Matrix 'A' does not have full rank.")
        Q[, k] <- Q[, k]/R[k, k]
    }
    Q # %*% R
}
