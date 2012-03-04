antsApplyTransforms <- function(...){
	.Call( "antsApplyTransforms", c(...) , PACKAGE = "ANTsR" ) ;
}
