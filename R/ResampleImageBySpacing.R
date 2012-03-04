ResampleImageBySpacing <- function(...){
	.Call( "ResampleImageBySpacing", c(...) , PACKAGE = "ANTsR" ) ;
}
