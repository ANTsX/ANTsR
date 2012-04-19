AverageImages <- function(...){
	.Call( "AverageImages", as.character( c(...) ) ) ;
}
