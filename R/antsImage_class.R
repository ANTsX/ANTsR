# this file defines the S4 classes 'antsImage' and 'antsImageList'

setClass( Class = "antsImage" , 
	  representation( pixeltype = "character" , # C++ type used to represent a pixel of image pointed to by 'pointer'
	  		  dimension = "numeric" , # dimension of the image pointerd to by 'pointer'
			  pointer = "externalptr" # pointer to the actual image of C++ type 'itk::image< pixeltype , dimension >::Pointer'
			  ) 
	  )

setMethod( f = "initialize" ,
	   signature( .Object = "antsImage"
		      ) ,
	   definition = function( .Object , 
	   	      		  pixeltype , 
				  dimension 
				  )
	   	       {
			 .Call( "antsImage", 
			 	pixeltype , 
				dimension 
				)
	   	       }
	   )


setClass( Class = "antsImageList" ,
	  representation( pixeltype = "character" ,
	  		  dimension = "numeric" ,
			  pointer = "externalptr"

	  		  )
	  )

setMethod( f = "initialize" ,
	   signature( .Object = "antsImageList"
		      ) ,
	   definition = function( .Object , 
	   	      		  pixeltype , 
				  dimension 
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
