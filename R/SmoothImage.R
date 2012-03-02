SmoothImage <- function(...){
	.Call( "SmoothImage", c(...) , PACKAGE = "Ritk" ) ;
}
