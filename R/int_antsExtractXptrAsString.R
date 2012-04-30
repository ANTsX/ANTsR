setGeneric( name = "int_antsExtractXptrAsString" , 
	    def = function( object ) standardGeneric( "int_antsExtractXptrAsString" ) 
	    )

setMethod( f = "int_antsExtractXptrAsString" ,
	   signature = c( "antsImage" ) ,
	   definition = function( object )
	   	      	{
			  return( as.character( c( object@pointer ) ) ) 
			}
	   )

setMethod( f = "int_antsExtractXptrAsString" ,
	   signature = c( "antsImageList" ) ,
	   definition = function( object )
	   	      	{
			  return( paste( as.character( c( object@pointer ) ) , "%" , sep = "" )  )
			}
	   )

setMethod( f = "int_antsExtractXptrAsString" ,
	   signature = c( "antsMatrix" ) ,
	   definition = function( object )
	   	      	{
			  return( as.character( c( object@pointer ) ) ) 
			}
	   )

setMethod( f = "int_antsExtractXptrAsString" ,
	   signature = c( "numeric" ) ,
	   definition = function( object )
	   	      	{
			  return( object )
			}
	   )

setMethod( f = "int_antsExtractXptrAsString" ,
	   signature = c( "character" ) ,
	   definition = function( object )
	   	      	{
			  return( object )
			}
	   )

