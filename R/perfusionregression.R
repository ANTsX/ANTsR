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
  visitvals<-((1:(ncol(mat)/skip))*skip)
  cbfform<-formula(  mat[,vox] ~   xideal + nuis )
  cl<-makeForkCluster(nnodes = getOption("mc.cores", 2L))
  registerDoParallel(cl,cores=getOption("mc.cores", 2L))
  ptime <- system.time({
  rgw<-foreach(vox=visitvals,.combine="+",.init=regweights,.verbose=F) %dopar% {
    mycbfmodel<-lmrob( cbfform , control = ctl ) # try(...,silent=T) 
    rbetaideal[vox]<-mycbfmodel$coeff[2]
    if ( vox %% 2 == 0 ) print(paste("robust regression:",vox/ncol(mat)*100,"%",ct))
    # regweights<-regweights+
    mycbfmodel$weights
    }
  })
  regweights<-(rgw/length(visitvals))
  print(paste("donewithrobreg",length(visitvals)))
  print(regweights)
  print(paste(ptime))
  # now use the weights in a weighted regression
  regweights[ regweights < 0.999995 ]<-0 # hard thresholding 
  cbfform<-formula(  mat ~   xideal + nuis )
  mycbfmodel<-lm( cbfform , weights = regweights ) # standard weighted regression
  betaideal<-(mycbfmodel$coeff)[2,]
  cbfi[ mask_img == 1 ]<-betaideal  # robust results
  }
# antsImageWrite(cbfi,"cbfregression.nii")

return( cbfi )
}
