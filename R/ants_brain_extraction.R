ants_brain_extraction <- function( img = "" )
{
	# check if called with no arguments and print usage
	if( nchar( img ) == 0 )
	{
		print( "usage: ants_brain_extraction( <time-series-average-image> )" ) ;
		return ;
	}

	# check if there is an extension
	if( length( strsplit( img , "." , fixed = TRUE )[[1]] ) < 2 )
	{
		print( "There appears to be no extension to the input file. Please provide a [nii|nii.gz] file." ) ;
		return ;
	}

	# split the string into filename and extension
	split_img = strsplit( img , "." , fixed = TRUE )[[1]] ;
	filename = split_img[1] ;
	if( length( split_img ) == 2 )
	{
		extension = paste( "" , split_img[2] , sep = "." ) ;
	}
	else if( length( split_img ) == 3 )
	{
		extension = paste( "" , split_img[2] , split_img[3] , sep = "." ) ;
	}
	
	bm_img = paste( filename , "_brainmask" , extension , sep = "" ) ;

	# N3BiasFieldCorrection( 3 , img , img , 2 ) ;
	# for( x in 1:3 )
	# {
	#	N3BiasFieldCorrection( 3 , img , img , 1 ) ;
	# }

	ThresholdImage( 3 , img , bm_img , "Otsu" , 3 ) ;
	ThresholdImage( 3 , bm_img , bm_img , 2 , 3 ) ;
	ImageMath( 3 , bm_img , "ME" , bm_img , 1 ) ;
	ImageMath( 3 , bm_img , "GetLargestComponent" , bm_img ) ;
	ImageMath( 3 , bm_img , "MD" , bm_img , 1 ) ;
	ImageMath( 3 , bm_img , "ME" , bm_img , 1 ) ;
}

get_mask <- function( img , thresh_lo , thresh_hi )
{
if( is.character( img ) )
{
  img <- antsImageRead( img , "float" , 3 )
}else if( class( img ) == "antsImage" )
{
  if( img@pixeltype != "float" || img@dimension != 3 ) 
  {
    print( "'img' must have pixeltype 'float' and dimension '3'" )
    return( NULL )
  }
}else
{
  print( "'img' must be a filename or an 'antsImage'" )
  return( NULL )
}

if( ( !is.numeric( thresh_lo ) && !is.integer( thresh_lo ) ) || ( !is.numeric( thresh_hi ) && !is.integer( thresh_hi ) ) )
{
  print( "'thresh_lo' & 'thresh_hi' must be scalars" )
  return( NULL )
}

mask_img <- new( "antsImage" , "float" , 3 )
ThresholdImage( 3 , img , mask_img , thresh_lo , thresh_hi )
ImageMath( 3 , mask_img , "ME" , mask_img , 2 )
ImageMath( 3 , mask_img , "GetLargestComponent" , mask_img )
ImageMath( 3 , mask_img , "MD" , mask_img , 1 )

return( mask_img )
}