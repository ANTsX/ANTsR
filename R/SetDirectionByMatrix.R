SetDirectionByMatrix <- function(...){
	.Call( "SetDirectionByMatrix", c(...) , PACKAGE = "ANTsR" ) ;
}
