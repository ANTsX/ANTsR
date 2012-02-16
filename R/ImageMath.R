ImageMath <- function(...){
	.Call( "ImageMath", c(...) , PACKAGE = "Ritk" ) ;
}
