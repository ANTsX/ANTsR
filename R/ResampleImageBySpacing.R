ResampleImageBySpacing <- function(...){
	.Call( "ResampleImageBySpacing", c(...) , PACKAGE = "Ritk" ) ;
}
