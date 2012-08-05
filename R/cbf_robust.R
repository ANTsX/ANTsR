cbf_robust <- function( asl_img )
{
getPckg <- function(pckg) install.packages(pckg, repos = "http://cran.r-project.org")

# FIXME --- should convert this section to a separate function : motion correction
img <- antsImageRead( asl_img , "float" , 4 )
avg_img = new( "antsImage" , "float" , 3 )
antsMotionCorr( list( d = 3 , a = img , o = avg_img ) )
moco_img = new( "antsImage" , "float" , 4 )
moco_params = new( "antsMatrix" , "double" )
antsMotionCorr( list( d = 3 , o = list( moco_params , moco_img , avg_img ) , m = list( name = "MI" , avg_img , img , 1 , 32 , "regular" , 0.05 ) , t = "Affine[1]" , i = 50 , u = 1 , e = 1 , s = 1 , f = 1 , n = 10 , l = 0 ) )
moco_mask_img <- new( "antsImage" , "float" , 3 )
# endfunction
# FIXME --- should convert this to a separate function : get mask
# add relevant options for user : lo and hi thresh
ThresholdImage( 3 , avg_img , moco_mask_img , 500 , 1000000000 )
ImageMath( 3 , moco_mask_img , "ME" , moco_mask_img , 2 )
ImageMath( 3 , moco_mask_img , "GetLargestComponent" , moco_mask_img )
ImageMath( 3 , moco_mask_img , "MD" , moco_mask_img , 1 )
logmask <- ( moco_mask_img == 1 )
# endfunction
# FIXME --- should convert this to a separate function : get matrix from masked time series
mat <- moco_img[ logmask ]
dim( mat ) <- c( sum( logmask == 1 ) , dim( moco_img )[4] )
mat <- t( mat )
# endfunction

# FIXME --- should convert this to a separate function : get perfusion predictors
# add relevant options for user :
#    . allow user to pass ideal signal in 
#    . allow user to  select either c(0,1) signal or c(1,0) signal
#    . allow user to select ncompcorparameters 
# set idealized signal
xideal<-( rep(c(0,1),dim(moco_img)[4])[1:dim(moco_img)[4]]-0.5  ) # control minus tag

# get nuisance variables : motion, compcor, etc
motionparams <- as.data.frame( moco_params ) 
motionnuis<-t(motionparams)[2:ncol( motionparams ) , ] # matrix elements
globalsignal<-residuals( lm( rowMeans(mat) ~ xideal ) )
nuis<-t( rbind(globalsignal, motionnuis )  )

# compute temporal variance of each column and apply CompCor
temporalvar<-apply(mat, 2, var)
tvhist<-hist(temporalvar , breaks = 20, plot=FALSE)
percvar<-0.015 # percentage of high variance data to use
thresh<-tvhist$mids[  cumsum( rev( tvhist$counts / sum(tvhist$counts) > percvar ) ) == T ]
wh<-( temporalvar > thresh )
highvarmat<-mat[,wh]
compcorrsvd<-svd( highvarmat %*% t( highvarmat ) )
ncompcorparameters<-4
if ( ncompcorparameters > 0  )
  {
  compcorr<-t( compcorrsvd$u[1:ncompcorparameters, ] )
  nuis<-cbind(nuis,compcorr)
  }
# endfunction

# FIXME --- should convert this to a separate function : perfusionregression
# add relevant options for user :
#    . allow user to optionally perform robust regression
# basic procedure, not robust
print("standard regression")
cbfform<-formula(  mat ~   xideal + nuis )
mycbfmodel<-lm( cbfform  ) # standard regression
betaideal<-(mycbfmodel$coeff)[2,]
cbfi <- antsImageClone( moco_mask_img )
cbfi[ logmask ]<-betaideal
#antsImageWrite(cbfi,"cbfbasic.nii")

dorobust <- F
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
  cbfi[ logmask ]<-betaideal  # robust results
  }
# antsImageWrite(cbfi,"cbfregression.nii")

# FIXME --- should convert this to a separate function : computecbf
# FIXME! this should be the cbf conversion factor
# you should allow different options, PASL, CASL, etc
# relevant parameters should be passed by command line
cbfmultiplicationfactor<-1   
cbfi[ logmask ]<-betaideal * cbfmultiplicationfactor

return( cbfi )
}
