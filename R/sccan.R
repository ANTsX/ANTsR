sccan <- function(...){
	.Call( "sccan", c(...) , PACKAGE = "Ritk" ) ;
}
