WarpTensorImageMultiTransform <- function(...){
	.Call( "WarpTensorImageMultiTransform", as.character( c(...) ) ) ;
}
