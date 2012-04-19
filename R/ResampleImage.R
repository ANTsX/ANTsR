ResampleImage <- function(...){
	.Call( "ResampleImage", as.character( c(...) ) ) ;
}
