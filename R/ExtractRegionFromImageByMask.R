ExtractRegionFromImageByMask <- function(...){
	.Call( "ExtractRegionFromImageByMask", c(...) , PACKAGE = "ANTsR" ) ;
}
