ConvertVectorFieldToVTK <- function(...){
	.Call( "ConvertVectorFieldToVTK", c(...) , PACKAGE = "ANTsR" ) ;
}

