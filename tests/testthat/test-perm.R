library(ANTsR)
set.seed(42)
n <- 40
p1 <- 15
p2 <- 15
k <- 2
U_true <- matrix(rnorm(n * k), n, k)
X1 <- U_true %*% matrix(rnorm(k * p1), k, p1) + matrix(rnorm(n * p1, sd = 0.1), n, p1)
X2 <- U_true %*% matrix(rnorm(k * p2), k, p2) + matrix(rnorm(n * p2, sd = 0.1), n, p2)
matlist <- list(X1 = X1, X2 = X2)

initu = initializeSimlr( matlist, 3, jointReduction = TRUE  )
myperm = simlr.perm(  matlist,
  energyType='acc',
  constraint='none',scale='centerAndScale', 
  mixAlg='pca',
  initialUMatrix=initu,
  iterations=5,
  nperms=2, FUN=rvcoef, verbose=FALSE )

print(head(myperm$significance))
