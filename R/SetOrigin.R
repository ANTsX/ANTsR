SetOrigin <- function(...){
	.Call( "SetOrigin", c(...) , PACKAGE = "ANTsR" ) ;
}
