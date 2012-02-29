CreateImage <- function(...){
	.Call( "CreateImage", c(...) , PACKAGE = "Ritk" ) ;
}
