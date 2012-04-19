ImageSetStatistics <- function(...){
	.Call( "ImageSetStatistics", as.character( c(...) ) ) ;
}
