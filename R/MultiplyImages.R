MultiplyImages <- function(...){
	.Call( "MultiplyImages", as.character( c(...) ) ) ;
}
