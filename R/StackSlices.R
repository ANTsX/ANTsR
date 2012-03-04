StackSlices <- function(...){
	.Call( "StackSlices", c(...) , PACKAGE = "ANTsR" ) ;
}
