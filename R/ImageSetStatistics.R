ImageSetStatistics <- function(...){
	.Call( "ImageSetStatistics", c(...) , PACKAGE = "ANTsR" ) ;
}
