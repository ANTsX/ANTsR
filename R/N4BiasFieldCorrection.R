N4BiasFieldCorrection <- function(...){
	.Call( "N4BiasFieldCorrection", int_antsProcessArguments(c(...)) , package = "libRN4BiasFieldCorrection" )
      }
