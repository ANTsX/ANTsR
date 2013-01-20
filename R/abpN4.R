abpN4 <- function( img = NA, intensityTruncation=c( 0.025, 0.975, 256 ), mask=NA usen3=FALSE )
  {
  numargs<-nargs()
  if ( numargs < 1 | missing(img) )
    {
    cat(" abpN4( img = inimg , intensityTruncation=c( 0.025, 0.975, 256 )\n")
    return(0);
    }
  if ( length( intensityTruncation ) != 3 )
    {
    cat("length( intensityTruncation ) should = 3 \n")
    return(1);
    }
  outimg<-antsImageClone( img , 'float' )
  dim<-as.character(img@dimension)
  ImageMath( dim,outimg,'TruncateImageIntensity',outimg,intensityTruncation[1],intensityTruncation[2],intensityTruncation[3])
  if ( usen3 == TRUE ) {
    N3BiasFieldCorrection(list(img@dimension, outimg, outimg, "4"))
    N3BiasFieldCorrection(list(img@dimension, outimg, outimg, "2"))
    return( outimg )
  }
  N4_CONVERGENCE_1<-"[50x50x50x50,0.0000001]"
  N4_CONVERGENCE_2<-"[20x20x20x20,0.0000001]"
  N4_SHRINK_FACTOR_1<-"4"
  N4_SHRINK_FACTOR_2<-"2"
  N4_BSPLINE_PARAMS<-"[200]"
  oimg<-substr( int_antsProcessArguments( list( outimg ) ) , 11 , 21 )
  if ( is.na( mask ) )
    {
    N4BiasFieldCorrection( list( d = outimg@dimension, i = oimg, s = N4_SHRINK_FACTOR_1, c = N4_CONVERGENCE_1, b = N4_BSPLINE_PARAMS, o = oimg ) )
    return( outimg )
    }
  N4BiasFieldCorrection( list( d = outimg@dimension, i = oimg, s = N4_SHRINK_FACTOR_1, c = N4_CONVERGENCE_1, b = N4_BSPLINE_PARAMS, x = mask, o = oimg ) )
  N4BiasFieldCorrection( list( d = outimg@dimension, i = oimg, s = N4_SHRINK_FACTOR_2, c = N4_CONVERGENCE_2, b = N4_BSPLINE_PARAMS, x = mask, o = oimg ) )
  return( outimg )
}
