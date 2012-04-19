antsMotionCorr <- function(...){
	.Call( "antsMotionCorr", as.character( c(...) ) ) ;
}
