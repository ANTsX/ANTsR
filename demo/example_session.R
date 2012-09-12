require(doParallel)
# image is in the ANTsR directory on mnt build avants 
asl_img<-'asl.nii.gz'
asl_img<-"JJ_asl_PEDS021.nii.gz"



asl<-"20100810_113600ep2dpcaslPHC1500msP2s002a001.nii.gz"
asl<-"20100812_070450ep2dpcaslUIPHCs021a001.nii.gz"
moco_results <- motion_correction( asl )
moco_mask_img <- get_mask( moco_results$moco_avg_img , thresh_lo = 500 , thresh_hi = 1e9 )
mat <- timeseries2matrix( moco_results$moco_img , moco_mask_img )
motionparams<-as.data.frame( moco_results$moco_params )
predictors <- get_perfusion_predictors( mat , motionparams)
cbf <- perfusionregression( moco_mask_img, mat , predictors$xideal , predictors$nuis )
# look at the results --- this is currently what we can use in a study
# if we control for global mean perfusion --- it's a relative measure 
antsImageWrite(cbf,"cbf.nii.gz") 
                                        # can also do robust cbf
cbfr <- perfusionregression( moco_mask_img, mat , predictors$xideal , predictors$nuis , TRUE )
antsImageWrite(cbfr,"cbfr.nii.gz") 

# network analysis
# draw the roi in label 2 on the mask
mask<-antsImageRead('021seg.nii.gz','float',3)
wb<-( mask > 1 )
nwb<-( mask <= 1 )
wbvec<-mask[ wb ]
roivec<-( wbvec > 3  ) 
wbvec[]<-1
mat <- timeseries2matrix( moco_results$moco_img , wb  )
mat<-residuals( lm( mat ~ predictors$xideal + predictors$nuis ) )
# mat<-residuals( lm( mat ~ predictors$nuis ) )
freqLo<-0.05
freqHi<-0.39
voxLo=round((1/freqLo)) # remove anything below this (high-pass)
voxHi=round((1/freqHi))   # keep anything above this
tr<-4;  myTimeSeries<-ts(mat,frequency=1.0/tr)
filteredTimeSeries<-residuals(cffilter(myTimeSeries,pl=voxHi,pu=voxLo,drift=T,type="t"))
spec.pgram( filteredTimeSeries[,vv], taper=0, fast=FALSE, detrend=F,demean=F, log="n")
roimeanval<-apply( filteredTimeSeries[,roivec], 1, mean )
ntwkmodel<-lm( mat ~ roimeanval ) 
ntwkbeta<-(ntwkmodel$coeff)[2,]
for ( i in 1:ncol(mat) ) ntwkbeta[i]<-cor.test(roimeanval,mat[,i])$p.value
qv<-1-p.adjust(ntwkbeta,method='BH')
mask[wb]<-as.real( qv >= 0.95 )
mask[nwb]<-0
antsImageWrite(mask  ,'021_beta.nii.gz')


# finally, try to convert to 'true' cbf values - not verified for all asl
# types!  this assumes CASL 
dd<-( moco_mask_img  > 0 )
mm<-antsImageRead("../m0.nii.gz",'float',3)
cbf2<-cbf[ dd ]*(-1) # tag-label difference from assumptions above 
# M0 <- 0.025 # rowMeans( controlimg ) --- can be a better way
lambda <- 0.9
alpha <- 0.68
T1b <- 1664
omega <- 1
tau <- 2
realcbf <- ( lambda * cbf2 ) / ( 2 * alpha * M0 * T1b * ( exp( -omega / T1b ) - exp( -( tau + omega ) / T1b ) ) )
max(realcbf)
cbf[dd]<-realcbf
antsImageWrite(cbf,"cbf.nii.gz") # look at the results


# pasl
	lambda <- 0.9
	alpha <- 0.95
	TI1 <- 700
	TI2 <- 1700
	T1b <- 1664
	pcbf <- ( lambda * cbf2 ) / ( 2 * alpha * mmx * TI1 * exp( -TI2 / T1b ) )

