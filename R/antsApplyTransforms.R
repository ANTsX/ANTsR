antsApplyTransforms <- function(...){
	.Call( "antsApplyTransforms", c(...) , PACKAGE = "Ritk" ) ;
}
