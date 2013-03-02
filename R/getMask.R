getMask <- function( img = NULL, lowThresh = 1 , highThresh = Inf, cleanup = FALSE) {
  #
  # Binarizes a mask between specified thresholds
  #
  # Input can be a file name or an antsImage, if it is not specified, a file chooser is launched. Works on 3D images only
  #
  # If cleanup == TRUE, small and weakly-connected elements are removed by erosion, and then holes are filled.
  #
  # Returns: a binary antsImage
  #
  
  if( is.character( img ) ) {
    if( length(img) != 1 ) {
      stop( "'img' must be a single filename" )
    }
    img <- antsImageRead( img , 3, "float" )
  }
  else if( class( img ) == "antsImage" ) {
    if(  img@dimension != 3 ) {
      stop( "'img' must have dimension '3'" )
    }
    if( img@pixeltype != "float" ) {
      img<-antsImageClone(img, 'float')
    }
  }
  else {
    img = file.choose() 
  }

  if( ( !is.numeric( lowThresh ) ) || ( !is.numeric( highThresh ) ) || length(lowThresh) > 1 || length(highThresh) > 1 ) {
    stop( "'lowthresh' and 'highthresh' must be numeric scalars" )
  }

  
  mask_img <- new( "antsImage" , "float" , 3 )

  ThresholdImage( 3 , img , mask_img , lowThresh ,  highThresh)

  if (cleanup) {
    ImageMath( 3 , mask_img , "ME" , mask_img , 2 )
    ImageMath( 3 , mask_img , "GetLargestComponent" , mask_img )
    ImageMath( 3 , mask_img , "MD" , mask_img , 1 )
    ImageMath( 3 , mask_img , "FillHoles" , mask_img  )
  }
    
  return( mask_img )
  
}
