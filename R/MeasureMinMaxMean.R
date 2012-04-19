MeasureMinMaxMean <- function(...){
	.Call( "MeasureMinMaxMean", as.character( c(...) ) ) ;
}
