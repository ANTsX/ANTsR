WarpTensorImageMultiTransform <- function(...){
	.Call( "WarpTensorImageMultiTransform", c(...) , PACKAGE = "ANTsR" ) ;
}
