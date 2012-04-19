ConvertScalarImageToRGB <- function(...){
	.Call( "ConvertScalarImageToRGB", as.character( c(...) ) ) ;
}
