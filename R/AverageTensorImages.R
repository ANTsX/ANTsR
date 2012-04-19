AverageTensorImages <- function(...){
	.Call( "AverageTensorImages", as.character( c(...) ) ) ;
}
