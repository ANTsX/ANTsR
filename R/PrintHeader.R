PrintHeader <- function(...){
	.Call( "PrintHeader", c(...) , PACKAGE = "Ritk" ) ;
}
