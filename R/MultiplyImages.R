MultiplyImages <- function(...){
	.Call( "MultiplyImages", c(...) , PACKAGE = "ANTsR" ) ;
}
