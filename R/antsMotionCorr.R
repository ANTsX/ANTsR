antsMotionCorr <- function(...){
	.Call( "antsMotionCorr", c(...) , PACKAGE = "ANTsR" ) ;
}
