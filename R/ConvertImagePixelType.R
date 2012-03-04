ConvertImagePixelType <- function(...){
	.Call( "ConvertImagePixelType", c(...) , PACKAGE = "ANTsR" ) ;
}
