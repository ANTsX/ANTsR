WarpImageMultiTransform <- function(...){
	.Call( "WarpImageMultiTransform" , as.character( c(...) ) ) ;
}
