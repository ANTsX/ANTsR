AverageImages <- function(...){
	.Call( "AverageImages", c(...) , PACKAGE = "ANTsR" ) ;
}
