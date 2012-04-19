CreateImage <- function(...){
	.Call( "CreateImage", as.character( c(...) ) ) ;
}
