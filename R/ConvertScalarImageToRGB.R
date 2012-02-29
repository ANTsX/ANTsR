ConvertScalarImageToRGB <- function(...){
	.Call( "ConvertScalarImageToRGB", c(...) , PACKAGE = "Ritk" ) ;
}
