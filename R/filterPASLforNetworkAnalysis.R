filterPASLforNetworkAnalysis<- function( asl, mask, moco, tr, freqLo=0.01 , freqHi=0.1, smoother = 0, pre="" , moreaccurate = TRUE )
{ 
  pixtype<-"float"
  myusage<-"usage: filterfMRIforNetworkAnalysis( asl, tr, freqLo=0.01, freqHi = 0.1, cbfnetwork=c(\"BOLD,ASLCBF,ASLBOLD\"), smoother = 0 , outputprefix = NULL )"
  
  if ( nargs() == 0 )
    {
    print(myusage)
    return(NULL)
    }
if ( ! is.numeric( tr ) | missing( tr ) )
  {
  print ( tr )
  print("TR parameter is missing or not numeric type - is typically between 2 and 4 , depending on your fMRI acquisition")
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
  asl <- antsImageRead( asl ,  4  )
} else if( class( asl ) == "antsImage" )
{
  if( asl@pixeltype != pixtype )
    {
    print(paste( "'asl' must have pixeltype  ",pixtype ))
    asl<-antsImageClone( asl , pixtype )
    }
  if( asl@dimension != 4 ) 
  {
    print(paste( "'asl' must have pixeltype ",pixtype," and dimension '4'" ))
    return( NULL )  
  }
}else
{
  print( "'asl' must be a filename or an 'antsImage'" )
  return( NULL )
}

# Read in the brain mask
if( is.character( mask ) )
{
  if( length( mask ) != 1 )
    {
      print( "'mask' should be only one filename" )
      return( NULL )
    }
  mask <- antsImageRead( mask ,  3  )
} else if( class( mask ) == "antsImage" )
{
  if( mask@pixeltype != pixtype )
    {
    print(paste( "'mask' must have pixeltype  ",pixtype ))
    mask<-antsImageClone( mask , pixtype )
    }
  if( mask@dimension != 3 ) 
  {
    print(paste( "'mask' must have pixeltype ",pixtype," and dimension '3'" ))
    return( NULL )  
  }
}else
{
  print( "'mask' must be a filename or an 'antsImage'" )
  return( NULL )
}

if ( missing( asl ) )
{
  print("Missing first (image) parameter")
  print(myusage)
  return( NULL ) 
}

if ( missing( mask ) )
{
  print("Missing second (image) parameter")
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

print ( "Inputs verified" )

# network analysis
wb<-( mask > 0 ) # whole brain
if ( smoother > 0 )
  {
  print ( "Smoothing input image" )
  SmoothImage(4,asl,smoother,asl)
  }
print ( "Convert input to time series" )
ogmat <- timeseries2matrix( asl , wb  )

motionparams<-as.data.frame( moco )
originaltimes <- dim(motionparams)[1] 
pad <- (originaltimes - dim(ogmat)[1]) / 2
print( dim(motionparams))
print( dim(ogmat ))
print (paste( "Image was padded by ", pad ))
motionparams <- motionparams[(pad+1):(originaltimes-pad),]
print ( "Read motion correction parameters" );

print( pad )
print( dim(motionparams) )
print( dim(ogmat ) )

predictors <- get_perfusion_predictors( ogmat , motionparams, NULL, 1, 3 )
motionparamsandcompcorr<-predictors$nuis
ogmat<-residuals( lm( ogmat ~ motionparamsandcompcorr ))
gmat <-ogmat #

print ( "Time series converted to matrix" )

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
return( list( filteredTimeSeries = filteredTimeSeries , temporalvar = temporalvar ) )
}
