ImageSetStatistics <- function(...){
	.Call( "ImageSetStatistics", c(...) , PACKAGE = "Ritk" ) ;
}
