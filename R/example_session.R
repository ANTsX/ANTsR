
library(ANTsR)
# image is in the ANTsR directory on mnt build avants 
asl_img<-'asl.nii.gz'
moco_results <- motion_correction( asl_img )
moco_mask_img <- get_mask( moco_results$moco_avg_img , thresh_lo = 500 , thresh_hi = 1e9 )
mat <- timeseries2matrix( moco_results$moco_img , moco_mask_img )
motionparams<-as.data.frame( moco_results$moco_params )
predictors <- get_perfusion_predictors( mat , motionparams)
cbf <- perfusionregression( moco_mask_img, mat , predictors$xideal , predictors$nuis )
# look at the results --- this is currently what we can use in a study
# if we control for global mean perfusion --- it's a relative measure 
antsImageWrite(cbf,"cbf.nii.gz") 
# can also do robust cbf
cbf <- perfusionregression( moco_mask_img, mat , predictors$xideal , predictors$nuis , TRUE )

# finally, try to convert to 'true' cbf values - not verified for all asl
# types!  this assumes CASL 
dd<-( moco_mask_img  > 0 )
cbf2<-cbf[ dd ]*(-1) # tag-label difference from assumptions above 
M0 <- 0.025 # rowMeans( controlimg ) --- can be a better way
lambda <- 0.9
alpha <- 0.68
T1b <- 1664
omega <- 1
tau <- 2
realcbf <- ( lambda * cbf2 ) / ( 2 * alpha * M0 * T1b * ( exp( -omega / T1b ) - exp( -( tau + omega ) / T1b ) ) )
max(realcbf)
cbf[dd]<-realcbf
antsImageWrite(cbf,"cbf.nii.gz") # look at the results


