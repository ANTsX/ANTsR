WarpImageMultiTransform <- function(...){
	.Call( "WarpImageMultiTransform" , c(...) , PACKAGE = "Ritk" ) ;
}
