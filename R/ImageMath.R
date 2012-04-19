ImageMath <- function(...){
	.Call( "ImageMath", as.character( c(...) ) ) ;
}
