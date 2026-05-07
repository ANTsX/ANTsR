library(ANTsR)
n=100
p=10
Xc=matrix(rnorm(n*p),nrow=n)
Y=matrix(rnorm(n*p),nrow=n)
v=F
w=0.5
r='soft_polar'
max_iter=500
o='lars'
ini=c('armijo')
t0=Sys.time()
nsaR=nsa_flow( Y, Xc, w=w, retraction=r, optimizer=o, verbose=v, max_iter=max_iter )
t1=Sys.time()
nsaP1=nsa_flow_torch_ag( Y, Xc, w=w, retraction=r, optimizer=o , verbose=v, max_iter=max_iter, apply_nonneg=TRUE, plot=TRUE, initial_learning_rate=ini[1] )
t2=Sys.time()
nsaP2=nsa_flow_autograd( Y, Xc, w=w, retraction=r, optimizer=o , verbose=v, max_iter=max_iter, apply_nonneg=TRUE, plot=TRUE, lr_strategy='armijo' )
t3=Sys.time()
print( paste0( "R time: ", as.numeric( t1 - t0, units='secs' )  ) )
print( paste0( "Python time: ", as.numeric( t2 - t1, units='secs' )  ) )
print( paste0( "Python (full grad) time: ", as.numeric( t3 - t2, units='secs' )  ) )
nsaP1$plot
nsaP2$plot
invariant_orthogonality_defect(nsaP1$Y)
invariant_orthogonality_defect(nsaP2$Y)
