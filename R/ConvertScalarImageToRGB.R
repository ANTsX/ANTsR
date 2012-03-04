ConvertScalarImageToRGB <- function(...){
	.Call( "ConvertScalarImageToRGB", c(...) , PACKAGE = "ANTsR" ) ;
}
