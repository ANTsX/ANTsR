
library(reticulate)
library(ANTsR)
Y <- matrix(rnorm(1000), 100, 10)
Xc <- matrix(rnorm(1000), 100, 10)
Y=Y/norm(Y,"F")
Xc=Xc/norm(Xc,"F")
res <- nsa_flow_torch(Y, Xc, w=0.01, max_iter = 1000, initial_learning_rate=1e-3, verbose = TRUE)
print( cor.test( as.numeric( Xc), as.numeric( res$Y) ))
