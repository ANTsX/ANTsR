PrintHeader <- function(...){
	.Call( "PrintHeader", c(...) , PACKAGE = "ANTsR" ) ;
}
