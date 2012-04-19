ExtractSliceFromImage <- function(...){
	.Call( "ExtractSliceFromImage", as.character( c(...) ) ) ;
}
