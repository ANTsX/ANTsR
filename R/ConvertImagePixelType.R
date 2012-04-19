ConvertImagePixelType <- function(...){
	.Call( "ConvertImagePixelType", as.character( c(...) ) ) ;
}
