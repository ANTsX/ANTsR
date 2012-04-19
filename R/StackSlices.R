StackSlices <- function(...){
	.Call( "StackSlices", as.character( c(...) ) ) ;
}
