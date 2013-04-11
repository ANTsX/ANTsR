antsImageRead <- function( filename , dimension , pixeltype = "float" )
{
	if( class( filename ) != "character" || length( filename ) != 1 )
	{
		print( "'filename' argument must be of class 'character' and have length 1" )
		return( NULL )
	}
	if( class( pixeltype ) != "character" || length( pixeltype ) != 1 )
	{
		print( "'pixeltype' argument must be of class 'character' and have length 1" )
		return( NULL )
	}
	if( ( (class( dimension ) != "numeric") && (class( dimension ) != "integer")) || length( dimension ) != 1 )
	{
		print( "'dimension' argument must be of class 'numeric' and have length 1" )
		return( NULL )
	}
	rval<-( .Call( "antsImageRead", filename , pixeltype , dimension , PACKAGE = "libRantsImageRead") )
        gc()
        return( rval ) 
}
