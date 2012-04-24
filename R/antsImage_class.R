# this file defines the S4 class 'antsImage'

setClass( Class = "antsImage" , 
	  representation( pixeltype = "character" , # C++ type used to represent a pixel of image pointed to by 'pointer'
	  		  dimension = "numeric" , # dimension of the image pointerd to by 'pointer'
			  pointer = "externalptr" # pointer to the actual image of C++ type 'itk::image< pixeltype , dimension >::Pointer'
			  ) 
	  )
