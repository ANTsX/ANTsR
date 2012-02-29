AverageTensorImages <- function(...){
	.Call( "AverageTensorImages", c(...) , PACKAGE = "Ritk" ) ;
}
