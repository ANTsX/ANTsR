ReorientTensorImage <- function(...){
	.Call( "ReorientTensorImage", c(...) , PACKAGE = "ANTsR" ) ;
}
