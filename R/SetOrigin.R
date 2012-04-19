SetOrigin <- function(...){
	.Call( "SetOrigin", as.character( c(...) ) ) ;
}
