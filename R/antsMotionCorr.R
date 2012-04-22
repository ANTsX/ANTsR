antsMotionCorr <- function(...){
	.Call( "antsMotionCorr", int_antsProcessArguments( c(...) ) ) ;
}
