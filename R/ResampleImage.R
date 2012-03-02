ResampleImage <- function(...){
	.Call( "ResampleImage", c(...) , PACKAGE = "Ritk" ) ;
}
