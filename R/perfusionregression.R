perfusionregression <- function( mask_img , mat , xideal , nuis , dorobust = FALSE )
{
getPckg <- function(pckg) install.packages(pckg, repos = "http://cran.r-project.org")

print("standard regression")
cbfform<-formula(  mat ~   xideal + nuis )
mycbfmodel<-lm( cbfform  ) # standard regression
betaideal<-(mycbfmodel$coeff)[2,]
cbfi <- antsImageClone( mask_img )
cbfi[ mask_img == 1 ]<-betaideal
#antsImageWrite(cbfi,"cbfbasic.nii")

if ( dorobust )
  {
  pckg = try(require(robust))
  if(!pckg) 
    {
    cat("Installing 'robust' from CRAN\n")
    getPckg("robust")
    require("robust")
    }
  # robust procedure Yohai, V.J. (1987) High breakdown-point and high efficiency estimates for regression.  _The Annals of Statistics_ *15*, 642-65
  print("begin robust regression") ;
  ctl<-lmrob.control( "KS2011", max.it = 1000 )
  regweights<-rep(0,nrow(mat))
  rbetaideal<-rep(0,ncol(mat))
  vox<-1
  ct<-0
  skip<-20
  visitvals<-( skip:floor( (ncol(mat)-1) / skip ) ) * skip
  print( dim(nuis) )
  cbfform<-formula(  mat[,vox] ~   xideal + nuis  )
  mynodes<-round( detectCores() / 2 ) # round( getOption("mc.cores", 2L) / 2 )
  print( paste( "nodes:" , mynodes ) )
#  cl<-makeForkCluster( nnodes = mynodes )
#  registerDoParallel( cl , cores = mynodes ) 
  rgw<-regweights
  myct<-0
  ptime <- system.time({
#  rgw<-foreach(vox=visitvals,.combine="+",.init=regweights,.verbose=F) %dopar% {
  for ( vox in visitvals ) {
    try(  mycbfmodel<-lmrob( cbfform , control = ctl ) , silent=T )
    rbetaideal[vox]<-mycbfmodel$coeff[2]
    if ( ! is.null(   mycbfmodel$weights ) )
      {
      rgw<-rgw + mycbfmodel$weights
      myct<-myct+1 
      }
    }
  })
  regweights<-(rgw/myct)
  print(paste("donewithrobreg",myct))
  print(regweights)
  print(paste(ptime))
  # now use the weights in a weighted regression
  indstozero<-which( regweights < ( 0.999995*max(regweights ) ) )
  if ( length(which) < 10 ) 
    {
    indstozero<-which( regweights < ( 0.95 * max(regweights ) ) )
    }
  regweights[ indstozero ]<-0 # hard thresholding 
  print(regweights)
  cbfform<-formula(  mat ~   xideal + nuis )
  mycbfmodel<-lm( cbfform , weights = regweights ) # standard weighted regression
  betaideal<-(mycbfmodel$coeff)[2,]
  cbfi[ mask_img == 1 ]<-betaideal  # robust results
  }
return( cbfi )
}
