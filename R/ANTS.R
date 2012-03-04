ANTS <- function(...){
	.Call( "ANTS", c(...) , PACKAGE = "ANTsR" ) ;
}
