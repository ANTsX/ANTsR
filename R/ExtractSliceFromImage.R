ExtractSliceFromImage <- function(...){
	.Call( "ExtractSliceFromImage", c(...) , PACKAGE = "Ritk" ) ;
}
