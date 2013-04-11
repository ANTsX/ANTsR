N3BiasFieldCorrection <- function(...){
	.Call( "N3BiasFieldCorrection", int_antsProcessArguments( c(...) ) , package = "libRN3BiasFieldCorrection") ;
}
