MeasureMinMaxMean <- function(...){
	.Call( "MeasureMinMaxMean", c(...) , PACKAGE = "ANTsR" ) ;
}
