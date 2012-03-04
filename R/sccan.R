sccan <- function(...){
	.Call( "sccan", c(...) , PACKAGE = "ANTsR" ) ;
}
