sccan <- function(...){
	.Call( "sccan", as.character( c(...) ) ) ;
}
