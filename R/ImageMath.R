ImageMath <- function(...){
	.Call( "ImageMath", c(...) , PACKAGE = "ANTsR" ) ;
}
