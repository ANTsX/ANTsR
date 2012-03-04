ImageCompare <- function(...){
	.Call( "ImageCompare", c(...) , PACKAGE = "ANTsR" ) ;
}
