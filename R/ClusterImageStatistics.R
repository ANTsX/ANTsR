ClusterImageStatistics <- function(...){
	.Call( "ClusterImageStatistics", c(...) , PACKAGE = "ANTsR" ) ;
}
