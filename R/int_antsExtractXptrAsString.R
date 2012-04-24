setGeneric( name = "int_antsExtractXptrAsString" , 
	    def = function( image ) standardGeneric( "int_antsExtractXptrAsString" ) 
	    )

setMethod( f = "int_antsExtractXptrAsString" ,
	   signature = c( "antsImage" ) ,
	   definition = function( image )
	   	      	{
			  return( as.character( c( image@pointer ) ) ) 
			}
	   )

setMethod( f = "int_antsExtractXptrAsString" ,
	   signature = c( "numeric" ) ,
	   definition = function( image )
	   	      	{
			  return( image )
			}
	   )

setMethod( f = "int_antsExtractXptrAsString" ,
	   signature = c( "character" ) ,
	   definition = function( image )
	   	      	{
			  return( image )
			}
	   )

