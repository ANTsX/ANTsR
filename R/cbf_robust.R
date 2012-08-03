cbf_robust <- function( asl_img )
{
img <- antsImageRead( asl_img , "float" , 4 )
avg_img = new( "antsImage" , "float" , 3 )
antsMotionCorr( list( d = 3 , a = img , o = avg_img ) )
moco_img = new( "antsImage" , "float" , 4 )
moco_params = new( "antsMatrix" , "double" )
antsMotionCorr( list( d = 3 , o = list( moco_params , moco_img , avg_img ) , m = list( name = "MI" , avg_img , img , 1 , 32 , "regular" , 0.05 ) , t = "Affine[0.25]" , i = 50 , u = 1 , e = 1 , s = 1 , f = 1 , n = 10 , l = 0 ) )
moco_mask_img <- new( "antsImage" , "float" , 3 )
ThresholdImage( 3 , avg_img , moco_mask_img , 500 , 1000000000 )
ImageMath( 3 , moco_mask_img , "ME" , moco_mask_img , 2 )
ImageMath( 3 , moco_mask_img , "GetLargestComponent" , moco_mask_img )
ImageMath( 3 , moco_mask_img , "MD" , moco_mask_img , 1 )
logmask <- ( moco_mask_img == 1 )
mat <- moco_img[ logmask ]
dim( mat ) <- c( sum( logmask == 1 ) , dim( moco_img )[4] )
mat <- t( mat )

# set idealized signal
xideal<-( rep(c(0,1),dim(moco_img)[4])[1:dim(moco_img)[4]]-0.5  ) # control minus tag

# get nuisance variables : motion, compcor, etc
motionparams <- as.data.frame( moco_params ) )
motionnuis<-t(motionparams)[2:ncol( motionparams ) , ] # matrix elements
globalsignal<-residuals( lm( rowMeans(mat) ~ xideal ) )
nuis<-t( rbind(globalsignal, motionnuis )  )

# compute temporal variance of each column and apply CompCor
temporalvar<-apply(mat, 2, var)
tvhist<-hist(temporalvar , plot=FALSE)
percvar<-0.02 # percentage of high variance data to use
thresh<-tvhist$mids[  cumsum( rev( tvhist$counts / sum(tvhist$counts) > percvar ) ) == T ]
wh<-( temporalvar > thresh )
highvarmat<-mat[,wh]
compcorrsvd<-svd( highvarmat %*% t( highvarmat ) )
compcorr<-t( compcorrsvd$u[1:4, ] )
nuis<-cbind(nuis,compcorr)

# basic procedure, not robust
print("standard regression")
cbfform<-formula(  mat ~   xideal + nuis )
mycbfmodel<-lm( cbfform  ) # standard regression
betaideal<-(mycbfmodel$coeff)[2,]
cbfi <- antsImageClone( moco_mask_img )
cbfmultiplicationfactor<-1
cbfi[ logmask ]<-betaideal * cbfmultiplicationfactor
#antsImageWrite(cbfi,"cbfbasic.nii")

# robust procedure Yohai, V.J. (1987) High breakdown-point and high efficiency estimates for regression.  _The Annals of Statistics_ *15*, 642-65
 print("begin robust regression") ;
 vox<-1
 rbetaideal<-rep(0,ncol(mat))
  while ( vox < ncol(mat ) ) {
    if ( vox %% 1000 == 0 ) print(paste("robust regression:",vox/ncol(mat)*100,"%"))
    cbfform<-formula(  mat[,vox] ~   xideal + nuis )
    mycbfmodel<-lmRob( cbfform ) # try(...,silent=T)
    rbetaideal[vox]<-mycbfmodel$coeff[2]
    vox<-vox+1
    } # apply( mat, 2, my.function )
  cbfi[ logmask ]<-rbetaideal * cbfmultiplicationfactor # robust
#antsImageWrite(cbfi,"cbfregression.nii")

return( cbfi )
}