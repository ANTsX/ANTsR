CreateImage <- function(...){
	.Call( "CreateImage", c(...) , PACKAGE = "ANTsR" ) ;
}
