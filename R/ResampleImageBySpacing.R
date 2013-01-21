ResampleImageBySpacing <- function(...){
	.Call( "ResampleImageBySpacing",int_antsProcessArguments( c(...) ) ) ;
}
