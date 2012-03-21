CheckTopology <- function(...){
	.Call( "CheckTopology", c(...) , PACKAGE = "ANTsR" ) ;
}
