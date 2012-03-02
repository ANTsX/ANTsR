StackSlices <- function(...){
	.Call( "StackSlices", c(...) , PACKAGE = "Ritk" ) ;
}
