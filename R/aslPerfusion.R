aslPerfusion<- function( asl, maskThresh = 500 , moreaccurate = TRUE , dorobust = 0 , m0 = NA , skip = 20, mask = NA )
{ 
  pixtype<-"float"
  myusage<-args( aslPerfusion )
  if ( nargs() == 0 )
    {
    print(myusage)
    return(NULL)
    }
  if ( ! is.numeric( maskThresh ) )
    {
    print("maskThresh is not numeric type")
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
    }
    else if( class( asl ) == "antsImage" )
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
    }
    else
      {
      print( "'asl' must be a filename or an 'antsImage'" )
      return( NULL )
      }
  if ( missing( asl ) )
    {
      print("Missing first (image) parameter")
      print(myusage)
      return( NULL ) 
    }
  n <- length(dim(asl))
  if ( n != 4 )
    {
      print("input image must have dimension 4 ")
      return( NULL ) 
    }
  moco_results <- motion_correction( asl , moreaccurate = moreaccurate )
  moco_mask_img <- getMask( moco_results$moco_avg_img , lowThresh = maskThresh, highThresh = 1e9, cleanup = TRUE )
  if ( ! is.na(mask) ) moco_mask_img <- mask
  mat <- timeseries2matrix( moco_results$moco_img, moco_mask_img )
  if ( is.na( m0 ) )
    {
    print("Estimating m0 image from the mean of the control values - might be wrong for your data! please check!")
    m0vals <- apply( mat[c(1:(nrow(mat)/2))*2,] , 2 , mean ) # for T C T C , JJ data
    m0<-antsImageClone( moco_mask_img )
    m0[ moco_mask_img == 0 ]<-0
    m0[ moco_mask_img == 1 ]<-m0vals
    }
  motionparams<-as.data.frame( moco_results$moco_params )
  predictors <- get_perfusion_predictors( mat , motionparams, NULL, 1, 3 )
#  mat <- antsr_frequency_filter( mat , freqHi = 0.1 , freqLo = 0.01, tr = 4 )
  
  # Get average tagged image
  m1vals <- apply(  mat[c(1:(nrow(mat)/2))*2-1,] , 2 , mean ) # for T C T C , JJ data
  m1 <- antsImageClone( moco_mask_img )
  m1[ moco_mask_img == 0 ]<-0
  m1[ moco_mask_img == 1 ]<-m1vals
  print( colnames(predictors$nuis) )
  perfusion <- perfusionregression( mask_img = moco_mask_img, mat = mat , xideal = predictors$xideal , nuis = predictors$nuis , dorobust = dorobust , skip = skip )
  return( list( perfusion = perfusion , aslTimeSeries=mat,  xideal=predictors$xideal , nuisancevariables = predictors$nuis , mask =  moco_mask_img, m0=m0, m1=m1 ) )
}
