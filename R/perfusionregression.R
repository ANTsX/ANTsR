perfussionregression <- function( mask_img , mat , xideal , nuis , dorobust = FALSE )
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
  if(!pckg) {
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
  cbfform<-formula(  mat[,vox] ~   xideal + nuis )
  while ( vox < ncol( mat ) ) {
    mycbfmodel<-lmrob( cbfform , control = ctl ) # try(...,silent=T) 
    rbetaideal[vox]<-mycbfmodel$coeff[2]
    regweights<-regweights+mycbfmodel$weights
    if ( vox %% 4000 == 0 ) print(paste("robust regression:",vox/ncol(mat)*100,"%"))
    vox<-vox+1
    ct<-ct+1
    } 
  # now use the weights in a weighted regression
  regweights<-regweights / ct
  regweights[ regweights < 0.5 ]<-0 # hard thresholding 
  cbfform<-formula(  mat ~   xideal + nuis )
  mycbfmodel<-lm( cbfform , weights = regweights ) # standard weighted regression
  betaideal<-(mycbfmodel$coeff)[2,]
  cbfi[ mask_img == 1 ]<-betaideal  # robust results
  }
# antsImageWrite(cbfi,"cbfregression.nii")

return( cbfi )
}
