SetSpacing <- function(...){
	.Call( "SetSpacing", as.character( c(...) ) ) ;
}
