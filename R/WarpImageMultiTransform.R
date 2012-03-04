WarpImageMultiTransform <- function(...){
	.Call( "WarpImageMultiTransform" , c(...) , PACKAGE = "ANTsR" ) ;
}
