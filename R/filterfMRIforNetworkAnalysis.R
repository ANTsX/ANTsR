filterfMRIforNetworkAnalysis<- function( asl, tr , freqLo=0.01 , freqHi=0.1, cbfnetwork="ASLCBF", maskThresh = 500 , pre="" , moreaccurate = TRUE )
{
  pixtype<-"double"
  myusage<-"usage: filterfMRIforNetworkAnalysis( asl, tr, freqLo=0.01, freqHi = 0.1, cbfnetwork=c(\"BOLD,ASLCBF,ASLBOLD\") , maskThresh=500, outputprefix = NULL )"
  if ( nargs() == 0 )
    {
    print(myusage)
    return(NULL)
    }
if ( ! is.numeric( tr ) | missing( tr ) )
  {
  print("TR parameter is missing or not numeric type - is typically between 2 and 4 , depending on your fMRI acquisition")
  print(myusage)
  return(NULL)
  }
if ( ! is.numeric( maskThresh ) )
  {
  print("maskThresh is not numeric type")
  print(myusage)
  return(NULL)
  }
if ( ! is.numeric( freqLo ) | ! is.numeric( freqHi ) )
  {
  print("freqLo/Hi is not numeric type")
  print(myusage)
  return(NULL)
  }
if( is.character( asl ) )
{
  if( length( asl ) != 1 )
    {
      print( "'asl' should be only one filename" )
      return( NULL )
    }
  asl <- antsImageRead( asl , "double" , 4 )
} else if( class( asl ) == "antsImage" )
{
  if( asl@pixeltype != "double" )
    {
    print( "'asl' must have pixeltype  'double' " )
    }
  if( asl@dimension != 4 ) 
  {
    print( "'asl' must have pixeltype 'double' and dimension '4'" )
    return( NULL )  
  }
}else
{
  print( "'asl' must be a filename or an 'antsImage'" )
  return( NULL )
}
# if ( missing( pre ) )
# {
#   print("Missing output-prefix argument")
#   print(myusage)
#   return( NULL ) 
# }
if ( missing( asl ) )
{
  print("Missing first (image) parameter")
  print(myusage)
  return( NULL ) 
}
freqLo<-freqLo*tr
freqHi<-freqHi*tr
n <- length(dim(asl))
if ( n != 4 )
  {
  print("input image must have dimension 4 ")
  return( NULL ) 
  }
moco_results <- motion_correction( asl , moreaccurate = moreaccurate )
moco_mask_img <- get_mask( moco_results$moco_avg_img , thresh_lo = maskThresh, thresh_hi = 1e9 )
mat <- timeseries2matrix( asl , moco_mask_img )
motionparams<-as.data.frame( moco_results$moco_params )
predictors <- get_perfusion_predictors( mat , motionparams, NULL, 1, 3 )
# m0vals <- apply( mat[c(1:(nrow(mat)/2))*2,] , 2 , mean ) # for T C T C , JJ data
# m0<-antsImageClone( moco_mask_img )
# m0[ moco_mask_img == 0 ]<-0
# m0[ moco_mask_img == 1 ]<-m0vals
# cbf <- perfusionregression( moco_mask_img, mat , predictors$xideal , predictors$nuis , m0 )
if ( nchar(pre) > 1 ) {
  antsImageWrite(moco_mask_img,paste(pre,"mask.nii.gz",sep='')) 
  antsImageWrite(moco_results$moco_avg_img,paste(pre,"avg.nii.gz",sep=''))
#  antsImageWrite(cbf,paste(pre,"cbf.nii.gz",sep=''))
}
# network analysis
mask<-moco_mask_img
wb<-( mask > 0 ) # whole brain
ogmat <- timeseries2matrix( moco_results$moco_img , wb  )
motionparamsandcompcorr<-predictors$nuis
ogmat<-residuals( lm( ogmat ~ motionparamsandcompcorr ))
gmat <-ogmat # 
leftinds<-shift(c(1:nrow(gmat)),1)
rightinds<-shift(c(1:nrow(gmat)),-1)
if ( cbfnetwork == "ASLCBF" ) {
  # surround subtraction for cbf networks 
  gmat<-ogmat - 0.5 * ( ogmat[leftinds,] + ogmat[rightinds,] )
  taginds<-c(1:(nrow(gmat)/2))*2
  controlinds<-taginds-1
  gmat[controlinds,]<-gmat[controlinds,]*(-1) # ok! done w/asl specific stuff
  plot( apply( gmat[controlinds,],1, mean) , type='l')
}
if ( cbfnetwork == "ASLBOLD" ) {
  # surround addition for bold networks 
  gmat<-ogmat + 0.5 * ( ogmat[leftinds,] + ogmat[rightinds,] )
  plot( apply( gmat[controlinds,],1, mean) , type='l')
}
voxLo=round((1/freqLo)) # remove anything below this (high-pass)
voxHi=round((1/freqHi))   # keep anything above this
myTimeSeries<-ts(gmat,frequency=1.0/tr)
filteredTimeSeries<-residuals(cffilter(myTimeSeries,pl=voxHi,pu=voxLo,drift=T, root=TRUE))
vox<-round( ncol( filteredTimeSeries ) * 0.5 )#  a test voxel
spec.pgram( filteredTimeSeries[,vox], taper=0, fast=FALSE, detrend=F,demean=F, log="n")
temporalvar<-apply(filteredTimeSeries, 2, var)
wh<-which( temporalvar == 0 )
for ( x in wh ) {
  filteredTimeSeries[,x]<-sample( filteredTimeSeries , nrow(filteredTimeSeries ) )
}
return( list( filteredTimeSeries = filteredTimeSeries , mask = mask, temporalvar = temporalvar ) )
}
