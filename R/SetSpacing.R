SetSpacing <- function(...){
	.Call( "SetSpacing", c(...) , PACKAGE = "ANTsR" ) ;
}
