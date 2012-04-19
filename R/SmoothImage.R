SmoothImage <- function(...){
	.Call( "SmoothImage", as.character( c(...) ) ) ;
}
