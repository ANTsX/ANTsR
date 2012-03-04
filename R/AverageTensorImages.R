AverageTensorImages <- function(...){
	.Call( "AverageTensorImages", c(...) , PACKAGE = "ANTsR" ) ;
}
