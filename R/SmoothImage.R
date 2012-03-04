SmoothImage <- function(...){
	.Call( "SmoothImage", c(...) , PACKAGE = "ANTsR" ) ;
}
