ClusterImageStatistics <- function(...){
	.Call( "ClusterImageStatistics", as.character( c(...) ) ) ;
}
