ExtractSliceFromImage <- function(...){
	.Call( "ExtractSliceFromImage", c(...) , PACKAGE = "ANTsR" ) ;
}
