ConvertImagePixelType <- function(...){
	.Call( "ConvertImagePixelType", c(...) , PACKAGE = "Ritk" ) ;
}
