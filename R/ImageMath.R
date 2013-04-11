ImageMath <- function(...){
	.Call( "ImageMath", int_antsProcessArguments( c(...) ) , package = "libRImageMath" ) ;
}
