MeasureImageSimilarity <- function(...){
	.Call( "MeasureImageSimilarity", c(...) , PACKAGE = "ANTsR" ) ;
}
