ANTS <- function(...){
	.Call( "ANTS", as.character( c(...) ) ) ;
}
