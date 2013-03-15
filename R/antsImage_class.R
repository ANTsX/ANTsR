# this file defines the S4 classes related to 'antsImage' and their associated methods

setClass( Class = "antsRegion" ,
	  representation( index = "numeric" ,
	  		  size = "numeric"
	  		  )
	  )

setClass( Class = "antsImage" , 
	  representation( pixeltype = "character" , # C++ type used to represent a pixel of image pointed to by 'pointer'
	  		  dimension = "integer" , # dimension of the image pointerd to by 'pointer'
			  pointer = "externalptr" # pointer to the actual image of C++ type 'itk::image< pixeltype , dimension >::Pointer'
			  ) 
	  )

setMethod( f = "initialize" ,
	   signature( .Object = "antsImage"
		      ) ,
	   definition = function( .Object , 
	   	      		  pixeltype = "float" , 
				  dimension = 3
				  )
	   	       {
			 return( .Call( "antsImage", pixeltype , dimension ) )
	   	       }
	   )

setMethod( f = "dim" ,
	   signature( x = "antsImage"
	   	      ) ,
	   definition = function( x )
	   	      	{
			  return( .Call( "antsImage_dim" , x ) )
			}
	   )

setMethod( f = "as.numeric" ,
	   signature( x = "antsImage"
	   	      ) ,
	   definition = function( x ,
	   	      		  mask = logical() ,
				  region = new( "antsRegion" , index = integer() , size = integer() )
				  )
			{
			  if( typeof( mask ) != "logical" )
			  {
			    print( "'mask' provided is not of type 'logical'" )
			    return()
			  }
			  return( .Call( "antsImage_asVector" , x , mask , region ) )
			}
           )

setMethod( f = "as.matrix" ,
	   signature( x = "antsImage"
	   	      ) ,
           definition = function( x ,
	   	      		  mask = logical() ,
				  region = new( "antsRegion" , index = integer() , size = integer() )
				  )
			{
			  if( typeof( mask ) != "logical" )
			  {
			    print( "'mask' provided is not of type 'logical'" )
			    return()
			  }
			  if( x@dimension != 2 )
			  {
			    print( "image dimension must be 2" )
			    return()
			  }
			  return( .Call( "antsImage_asVector" , x , mask , region ) )
			}

	   )

setMethod( f = "as.array" ,
	   signature( x = "antsImage"
	   	      ) ,
           definition = function( x ,
	   	      		  mask = logical() ,
				  region = new( "antsRegion" , index = integer() , size = integer() )
				  )
			{
			  if( typeof( mask ) != "logical" )
			  {
			    print( "'mask' provided is not of type 'logical'" )
			    return()
			  }
			  return( .Call( "antsImage_asVector" , x , mask , region ) )
			}

	   )

setMethod( f = "[" ,
	   signature( x = "antsImage" ,
	   	      i = "NULL"
	   	      ) ,
	   definition = function( x ,
	   	      		  i 
				  )
			{
			  mask = logical(0)
			  region = new( "antsRegion" , index = integer() , size = integer() )
			  return( .Call( "antsImage_asVector" , x , mask , region ) )
			}
	   )

setMethod( f = "[" ,
	   signature( x = "antsImage" ,
	   	      i = "logical"
	   	      ) ,
	   definition = function( x ,
	   	      		  i 
				  )
			{
			  region = new( "antsRegion" , index = integer() , size = integer() )
			  return( .Call( "antsImage_asVector" , x , i , region ) )
			}
	   )

setMethod( f = "[" ,
	   signature( x = "antsImage" ,
	   	      i = "array"
	   	      ) ,
	   definition = function( x ,
	   	      		  i 
				  )
			{
			  if( typeof( i ) != "logical" )
			  {
			    print( "'mask' provided is not of type 'logical'" )
			    return()
			  }
			  region = new( "antsRegion" , index = integer() , size = integer() )
			  return( .Call( "antsImage_asVector" , x , i , region ) )
			}
	   )

setMethod( f = "[" ,
	   signature( x = "antsImage" ,
	   	      i = "matrix"
	   	      ) ,
	   definition = function( x ,
	   	      		  i 
				  )
			{
			  if( typeof( i ) != "logical" )
			  {
			    print( "'mask' provided is not of type 'logical'" )
			    return()
			  }
			  region = new( "antsRegion" , index = integer() , size = integer() )
			  return( .Call( "antsImage_asVector" , x , i , region ) )
			}
	   )

setMethod( f = "[" ,
	   signature( x = "antsImage" ,
	   	      i = "list"
	   	      ) ,
	   definition = function( x ,
	   	      		  i 
				  )
			{
			  if( class( i$mask ) == "NULL" )
			  {
			    i$mask = logical(0)
			  }
			  else if( typeof( i$mask ) != "logical" )
			  {
			    print( "'mask' provided is not of type 'logical' or 'NULL'" )
			    return()
			  }
			  if( class( i$region ) != "antsRegion" )
			  {
			    print( "'region' provided is not of class 'antsRegion'" )
			    return()
			  }
			  return( .Call( "antsImage_asVector" , x , i$mask , i$region ) )
			}
	   )

setMethod( f = "[" ,
	   signature( x = "antsImage" ,
	   	      i = "NULL" ,
		      j = "antsRegion"
	   	      ) ,
	   definition = function( x ,
	   	      		  i ,
				  j
				  )
			{
			  mask = logical(0)
			  return( .Call( "antsImage_asVector" , x , mask , j ) )
			}
	   )

setMethod( f = "[" ,
	   signature( x = "antsImage" ,
	   	      i = "logical" ,
		      j = "antsRegion"
	   	      ) ,
	   definition = function( x ,
	   	      		  i ,
				  j
				  )
			{
			  return( .Call( "antsImage_asVector" , x , i , j ) )
			}
	   )

setMethod( f = "[" ,
	   signature( x = "antsImage" ,
	   	      i = "array" ,
		      j = "antsRegion"
	   	      ) ,
	   definition = function( x ,
	   	      		  i ,
				  j
				  )
			{
			  if( typeof( i ) != "logical" )
			  {
			    print( "'mask' provided is not of type 'logical'" )
			    return()
			  }
			  return( .Call( "antsImage_asVector" , x , i , j ) )
			}
	   )

setMethod( f = "[" ,
	   signature( x = "antsImage" ,
	   	      i = "matrix" ,
		      j = "antsRegion"
	   	      ) ,
	   definition = function( x ,
	   	      		  i ,
				  j
				  )
			{
			  if( typeof( i ) != "logical" )
			  {
			    print( "'mask' provided is not of type 'logical'" )
			    return()
			  }
			  return( .Call( "antsImage_asVector" , x , i , j ) )
			}
	   )

antsGetPixels <- function( x , 
	  	     	   i = NA , 
		           j = NA , 
		           k = NA , 
		           l = NA 
		           )
{
lst = NULL
if( length( i ) !=1 || !is.na( i ) )
{
  if( is.null( i ) )
  {
    lst = c( lst , list( integer(0) ) )
  }
  else if( class( i ) == "integer" || class( i ) == "numeric" )
  {
    lst = c( lst , list( i ) )
  }
  else
  {
    print( "indices must be of class 'integer' or 'numeric'" )
    return()
  }
}

if( length( j ) !=1 || !is.na( j ) )
{
  if( is.null( j ) )
  {
    lst = c( lst , list( integer(0) ) )
  }
  else if( class( j ) == "integer" || class( j ) == "numeric" )
  {
    lst = c( lst , list( j ) )
  }
  else
  {
    print( "indices must be of class 'integer' or 'numeric'" )
    return()
  }
}

if( length( k ) != 1 || !is.na( k ) )
{
  if( is.null( k ) )
  {
    lst = c( lst , list( integer(0) ) )
  }
  else if( class( k ) == "integer" || class( k ) == "numeric" )
  {
    lst = c( lst , list( k ) )
  }
  else
  {
    print( "indices must be of class 'integer' or 'numeric'" )
    return()
  }
}

if( length( l ) != 1 || !is.na( l ) )
{
  if( is.null( l ) )
  {
    lst = c( lst , list( integer(0) ) )
  }
  else if( class( l ) == "integer" || class( l ) == "numeric" )
  {
    lst = c( lst , list( l ) )
  }
  else
  {
    print( "indices must be of class 'integer' or 'numeric'" )
    return()
  }
}
return( .Call( "antsImage_GetPixels" , x , lst ) )
}

antsGetSpacing <- function( x )
{
  return( .Call( "antsImage_GetSpacing" , x ) )
}

antsSetSpacing <- function( x, spacing )
{
  if ( class(spacing) != "numeric" )
    {
    print( "spacing must be of class 'numeric'" )
    return()
  }
  
  if ( length(spacing) != length(dim(x)) )
    {
    print( "spacing must be of same dimensions as image" )
    return()
    }
  
  return( .Call( "antsImage_SetSpacing", x, spacing ) )
}

antsGetOrigin <- function( x )
{
  return( .Call( "antsImage_GetOrigin" , x ) )
}

antsSetOrigin <- function( x, spacing )
{
  if ( class(spacing) != "numeric" )
    {
    print( "spacing must be of class 'numeric'" )
    return()
  }
  
  if ( length(spacing) != length(dim(x)) )
    {
    print( "spacing must be of same dimensions as image" )
    return()
    }
  
  return( .Call( "antsImage_SetOrigin", x, spacing ) )
}

antsGetDirection <- function( x )
{
  return( .Call( "antsImage_GetDirection", x ) )
}

setMethod( f = "[" ,
	   signature( x = "antsImage" ,
	   	      i = "NULL" ,
		      j = "NULL"
		      ) ,
	   definition = function( x ,
	   	        	  i ,
		       		  j ,
				  k = NA ,
				  l = NA
		       		  )
	     	        {
			  return( antsGetPixels( x , i , j , k , l ) )
	     		}
	   )

setMethod( f = "[" ,
	   signature( x = "antsImage" ,
	   	      i = "numeric" ,
		      j = "numeric"
		      ) ,
	   definition = function( x ,
	   	        	  i ,
		       		  j ,
				  k = NA ,
				  l = NA
		       		  )
	     	        {
			  return( antsGetPixels( x , i , j , k , l ) )
	     		}
	   )

setMethod( f = "[" ,
	   signature( x = "antsImage" ,
	   	      i = "numeric" ,
		      j = "NULL"
		      ) ,
	   definition = function( x ,
	   	        	  i ,
		       		  j ,
				  k = NA ,
				  l = NA
		       		  )
	     	        {
			  return( antsGetPixels( x , i , j , k , l ) )
	     		}
	   )

setMethod( f = "[" ,
	   signature( x = "antsImage" ,
	   	      i = "NULL" ,
		      j = "numeric"
		      ) ,
	   definition = function( x ,
	   	        	  i ,
		       		  j ,
				  k = NA ,
				  l = NA
		       		  )
	     	        {
			  return( antsGetPixels( x , i , j , k , l ) )
	     		}
	   )

setMethod( f = "[<-" ,
	   signature( x = "antsImage" ,
	   	      i = "NULL"
	   	      ) ,
	   definition = function( x ,
	   	      		  i ,
				  value 
				  )
			{
			  mask = logical(0)
			  region = new( "antsRegion" , index = integer() , size = integer() )
			  return( .Call( "antsImage_SetRegion" , x , mask , region , value ) )
			}
	   )

setMethod( f = "[<-" ,
	   signature( x = "antsImage" ,
	   	      i = "logical"
	   	      ) ,
	   definition = function( x ,
	   	      		  i ,
				  value 
				  )
			{
			  region = new( "antsRegion" , index = integer() , size = integer() )
			  return( .Call( "antsImage_SetRegion" , x , i , region , value ) )
			}
	   )

setMethod( f = "[<-" ,
	   signature( x = "antsImage" ,
	   	      i = "array"
	   	      ) ,
	   definition = function( x ,
	   	      		  i ,
				  value
				  )
			{
			  if( typeof( i ) != "logical" )
			  {
			    print( "'mask' provided is not of type 'logical'" )
			    return()
			  }
			  region = new( "antsRegion" , index = integer() , size = integer() )
			  return( .Call( "antsImage_SetRegion" , x , i , region , value ) )
			}
	   )

setMethod( f = "[<-" ,
	   signature( x = "antsImage" ,
	   	      i = "matrix"
	   	      ) ,
	   definition = function( x ,
	   	      		  i ,
				  value
				  )
			{
			  if( typeof( i ) != "logical" )
			  {
			    print( "'mask' provided is not of type 'logical'" )
			    return()
			  }
			  region = new( "antsRegion" , index = integer() , size = integer() )
			  return( .Call( "antsImage_SetRegion" , x , i , region , value ) )
			}
	   )

setMethod( f = "[<-" ,
	   signature( x = "antsImage" ,
	   	      i = "list"
	   	      ) ,
	   definition = function( x ,
	   	      		  i ,
				  value
				  )
			{
			  if( class( i$mask ) == "NULL" )
			  {
			    i$mask = logical(0)
			  }
			  else if( typeof( i$mask ) != "logical" )
			  {
			    print( "'mask' provided is not of type 'logical'" )
			    return()
			  }
			  if( class( i$region ) != "antsRegion" )
			  {
			    print( "'region' provided is not of class 'antsRegion'" )
			    return()
			  }
			  return( .Call( "antsImage_SetRegion" , x , i$mask , i$region , value ) )
			}
	   )

setMethod( f = "[<-" ,
	   signature( x = "antsImage" ,
	   	      i = "NULL" ,
		      j = "antsRegion"
	   	      ) ,
	   definition = function( x ,
	   	      		  i ,
				  j ,
				  value
				  )
			{
			  mask = logical(0)
			  return( .Call( "antsImage_SetRegion" , x , mask , j , value ) )
			}
	   )

setMethod( f = "[<-" ,
	   signature( x = "antsImage" ,
	   	      i = "logical" ,
		      j = "antsRegion"
	   	      ) ,
	   definition = function( x ,
	   	      		  i ,
				  j ,
				  value
				  )
			{
			  return( .Call( "antsImage_SetRegion" , x , i , j , value ) )
			}
	   )

setMethod( f = "[<-" ,
	   signature( x = "antsImage" ,
	   	      i = "array" ,
		      j = "antsRegion"
	   	      ) ,
	   definition = function( x ,
	   	      		  i ,
				  j ,
				  value
				  )
			{
			  if( typeof( i ) != "logical" )
			  {
			    print( "'mask' provided is not of type 'logical'" )
			    return()
			  }
			  return( .Call( "antsImage_SetRegion" , x , i , j , value ) )
			}
	   )

setMethod( f = "[<-" ,
	   signature( x = "antsImage" ,
	   	      i = "matrix" ,
		      j = "antsRegion"
	   	      ) ,
	   definition = function( x ,
	   	      		  i ,
				  j ,
				  value
				  )
			{
			  if( typeof( i ) != "logical" )
			  {
			    print( "'mask' provided is not of type 'logical'" )
			    return()
			  }
			  return( .Call( "antsImage_SetRegion" , x , i , j , value ) )
			}
	   )

antsSetPixels <- function( x , 
	  	     	   i = NA , 
		           j = NA , 
		           k = NA , 
		           l = NA ,
			   value
		           )
{
lst = NULL
if( length( i ) !=1 || !is.na( i ) )
{
  if( is.null( i ) )
  {
    lst = c( lst , list( integer(0) ) )
  }
  else if( class( i ) == "integer" || class( i ) == "numeric" )
  {
    lst = c( lst , list( i ) )
  }
  else
  {
    print( "indices must be of class 'integer' or 'numeric'" )
    return()
  }
}

if( length( j ) !=1 || !is.na( j ) )
{
  if( is.null( j ) )
  {
    lst = c( lst , list( integer(0) ) )
  }
  else if( class( j ) == "integer" || class( j ) == "numeric" )
  {
    lst = c( lst , list( j ) )
  }
  else
  {
    print( "indices must be of class 'integer' or 'numeric'" )
    return()
  }
}

if( length( k ) != 1 || !is.na( k ) )
{
  if( is.null( k ) )
  {
    lst = c( lst , list( integer(0) ) )
  }
  else if( class( k ) == "integer" || class( k ) == "numeric" )
  {
    lst = c( lst , list( k ) )
  }
  else
  {
    print( "indices must be of class 'integer' or 'numeric'" )
    return()
  }
}

if( length( l ) != 1 || !is.na( l ) )
{
  if( is.null( l ) )
  {
    lst = c( lst , list( integer(0) ) )
  }
  else if( class( l ) == "integer" || class( l ) == "numeric" )
  {
    lst = c( lst , list( l ) )
  }
  else
  {
    print( "indices must be of class 'integer' or 'numeric'" )
    return()
  }
}
return( .Call( "antsImage_SetPixels" , x , lst , value ) )
}

setMethod( f = "[<-" ,
	   signature( x = "antsImage" ,
	   	      i = "NULL" ,
		      j = "NULL" ,
		      value = "numeric"
		      ) ,
	   definition = function( x ,
	   	        	  i ,
		       		  j ,
				  ... ,
				  value
		       		  )
	     	        {
			  return( antsSetPixels( x , i , j , ... , value = value ) )
	     		}
	   )

setMethod( f = "[<-" ,
	   signature( x = "antsImage" ,
	   	      i = "numeric" ,
		      j = "numeric" ,
		      value = "numeric"
		      ) ,
	   definition = function( x ,
	   	        	  i ,
		       		  j ,
				  ... ,
				  value
		       		  )
	     	        {
			  return( antsSetPixels( x , i , j , ... , value = value ) )
	     		}
	   )

setMethod( f = "[<-" ,
	   signature( x = "antsImage" ,
	   	      i = "numeric" ,
		      j = "NULL" ,
		      value = "numeric"
		      ) ,
	   definition = function( x ,
	   	        	  i ,
		       		  j ,
				  ... ,
				  value
		       		  )
	     	        {
			  return( antsSetPixels( x , i , j , ... , value = value ) )
	     		}
	   )

setMethod( f = "[<-" ,
	   signature( x = "antsImage" ,
	   	      i = "NULL" ,
		      j = "numeric" ,
		      value = "numeric"
		      ) ,
	   definition = function( x ,
	   	        	  i ,
		       		  j ,
				  ... ,
				  value
		       		  )
	     	        {
			  return( antsSetPixels( x , i , j , ... , value = value ) )
	     		}
	   )

setGeneric( name = "as.antsImage" ,
	    def = function( object , ... ) standardGeneric( "as.antsImage" )
	    )

setMethod( f = "as.antsImage" ,
	   signature( object = "matrix" 
	   	      ) ,
	   definition = function( object , 
	   	      		  pixeltype = "double" , 
				  spacing = as.numeric( seq.int( from = 1 , 
				  	    			 by = 0 , 
								 length.out = length(dim( object ))
								 ) 
							) , 
				  origin = as.numeric( seq.int( from = 0 , 
				  	    			 by = 0 , 
								 length.out = length(dim( object )) 
								 ) 
							)
				  )
	   	      	{
			  return( .Call( "antsImage_asantsImage" , object , pixeltype , spacing , origin ) )
	   		}
	   )

setMethod( f = "as.antsImage" ,
	   signature( object = "array" 
	   	      ) ,
	   definition = function( object , 
	   	      		  pixeltype = "double" , 
				  spacing = as.numeric( seq.int( from = 1 , 
				  	    			 by = 0 , 
								 length.out = length(dim( object )) 
								 ) 
							) , 
				  origin = as.numeric( seq.int( from = 0 , 
				  	    			 by = 0 , 
								 length.out = length(dim( object )) 
								 ) 
							)
				  )
	   	      	{
			  return( .Call( "antsImage_asantsImage" , object , pixeltype , spacing , origin ) )
	   		}
	   )

setMethod( f = "==" ,
	   signature( e1 = "antsImage" 
	   	      ) ,
	   definition = function( e1 , 
	   	      		  e2 
				  )
	   	      	{
			  operator = "=="
			  if( class( e2 ) == "list" )
			  {
			    if( length( e2$value ) != 1 )
			    {
			      print( "length of value must be 1" )
			      return()
			    }
			    if( class( e2$region ) != "antsRegion" )
			    {
			      print( "region argument not of class 'antsRegion'" )
			      return()
			    }
			    return( .Call( "antsImage_RelationalOperators" , e1 , e2$value , e2$region , operator ) )
			  }
			  else if( class(e2) == "numeric" && length( e2 ) == 1 )
			  {
			    region = new( "antsRegion" , index = integer() , size = integer() )
			    return( .Call( "antsImage_RelationalOperators" , e1 , e2 , region , operator ) )
			  }
			  else
			  {
			    print( "rhs must be a scalar or a list( <scalar> , <antsRegion> )" )
			    return()
			  }
			}
	   )

setMethod( f = "!=" ,
	   signature( e1 = "antsImage" 
	   	      ) ,
	   definition = function( e1 , 
	   	      		  e2 
				  )
	   	      	{
			  operator = "!="
			  if( class( e2 ) == "list" )
			  {
			    if( length( e2$value ) != 1 )
			    {
			      print( "length of value must be 1" )
			      return()
			    }
			    if( class( e2$region ) != "antsRegion" )
			    {
			      print( "region argument not of class 'antsRegion'" )
			      return()
			    }
			    return( .Call( "antsImage_RelationalOperators" , e1 , e2$value , e2$region , operator ) )
			  }
			  else if( class(e2) == "numeric" && length( e2 ) == 1 )
			  {
			    region = new( "antsRegion" , index = integer() , size = integer() )
			    return( .Call( "antsImage_RelationalOperators" , e1 , e2 , region , operator ) )
			  }
			  else
			  {
			    print( "rhs must be a scalar or a list( <scalar> , <antsRegion> )" )
			    return()
			  }
			}
	   )

setMethod( f = "<=" ,
	   signature( e1 = "antsImage" 
	   	      ) ,
	   definition = function( e1 , 
	   	      		  e2 
				  )
	   	      	{
			  operator = "<="
			  if( class( e2 ) == "list" )
			  {
			    if( length( e2$value ) != 1 )
			    {
			      print( "length of value must be 1" )
			      return()
			    }
			    if( class( e2$region ) != "antsRegion" )
			    {
			      print( "region argument not of class 'antsRegion'" )
			      return()
			    }
			    return( .Call( "antsImage_RelationalOperators" , e1 , e2$value , e2$region , operator ) )
			  }
			  else if( class(e2) == "numeric" && length( e2 ) == 1 )
			  {
			    region = new( "antsRegion" , index = integer() , size = integer() )
			    return( .Call( "antsImage_RelationalOperators" , e1 , e2 , region , operator ) )
			  }
			  else
			  {
			    print( "rhs must be a scalar or a list( <scalar> , <antsRegion> )" )
			    return()
			  }
			}
	   )

setMethod( f = ">=" ,
	   signature( e1 = "antsImage" 
	   	      ) ,
	   definition = function( e1 , 
	   	      		  e2 
				  )
	   	      	{
			  operator = ">="
			  if( class( e2 ) == "list" )
			  {
			    if( length( e2$value ) != 1 )
			    {
			      print( "length of value must be 1" )
			      return()
			    }
			    if( class( e2$region ) != "antsRegion" )
			    {
			      print( "region argument not of class 'antsRegion'" )
			      return()
			    }
			    return( .Call( "antsImage_RelationalOperators" , e1 , e2$value , e2$region , operator ) )
			  }
			  else if( class(e2) == "numeric" && length( e2 ) == 1 )
			  {
			    region = new( "antsRegion" , index = integer() , size = integer() )
			    return( .Call( "antsImage_RelationalOperators" , e1 , e2 , region , operator ) )
			  }
			  else
			  {
			    print( "rhs must be a scalar or a list( <scalar> , <antsRegion> )" )
			    return()
			  }
			}
	   )

setMethod( f = "<" ,
	   signature( e1 = "antsImage" 
	   	      ) ,
	   definition = function( e1 , 
	   	      		  e2 
				  )
	   	      	{
			  operator = "<"
			  if( class( e2 ) == "list" )
			  {
			    if( length( e2$value ) != 1 )
			    {
			      print( "length of value must be 1" )
			      return()
			    }
			    if( class( e2$region ) != "antsRegion" )
			    {
			      print( "region argument not of class 'antsRegion'" )
			      return()
			    }
			    return( .Call( "antsImage_RelationalOperators" , e1 , e2$value , e2$region , operator ) )
			  }
			  else if( class(e2) == "numeric" && length( e2 ) == 1 )
			  {
			    region = new( "antsRegion" , index = integer() , size = integer() )
			    return( .Call( "antsImage_RelationalOperators" , e1 , e2 , region , operator ) )
			  }
			  else
			  {
			    print( "rhs must be a scalar or a list( <scalar> , <antsRegion> )" )
			    return()
			  }
			}
	   )

setMethod( f = ">" ,
	   signature( e1 = "antsImage" 
	   	      ) ,
	   definition = function( e1 , 
	   	      		  e2 
				  )
	   	      	{
			  operator = ">"
			  if( class( e2 ) == "list" )
			  {
			    if( length( e2$value ) != 1 )
			    {
			      print( "length of value must be 1" )
			      return()
			    }
			    if( class( e2$region ) != "antsRegion" )
			    {
			      print( "region argument not of class 'antsRegion'" )
			      return()
			    }
			    return( .Call( "antsImage_RelationalOperators" , e1 , e2$value , e2$region , operator ) )
			  }
			  else if( class(e2) == "numeric" && length( e2 ) == 1 )
			  {
			    region = new( "antsRegion" , index = integer() , size = integer() )
			    return( .Call( "antsImage_RelationalOperators" , e1 , e2 , region , operator ) )
			  }
			  else
			  {
			    print( "rhs must be a scalar or a list( <scalar> , <antsRegion> )" )
			    return()
			  }
			}
	   )

###################################################################################################

setClass( Class = "antsImageList" ,
	  representation( pixeltype = "character" ,
	  		  dimension = "integer" ,
			  pointer = "externalptr"

	  		  )
	  )

setMethod( f = "initialize" ,
	   signature( .Object = "antsImageList"
		      ) ,
	   definition = function( .Object , 
	   	      		  pixeltype = "float" , 
				  dimension = 3 
				  )
	   	       {
			 .Call( "antsImageList", 
			 	pixeltype , 
				dimension 
				)
	   	       }
	   )

setMethod( f = "as.list" ,
	   signature( x = "antsImageList" ) ,
	   definition = function( x )
	   	      {
			return( .Call( "antsImageList_asList" , x ) )
	   	      }
	   )
