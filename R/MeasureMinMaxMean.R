MeasureMinMaxMean <- function(...){
	.Call( "MeasureMinMaxMean", c(...) , PACKAGE = "Ritk" ) ;
}
