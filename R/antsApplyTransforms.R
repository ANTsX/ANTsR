antsApplyTransforms <- function(...){
	.Call( "antsApplyTransforms", as.character( c(...) ) ) ;
}
