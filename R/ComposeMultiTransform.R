ComposeMultiTransform <- function(...){
	.Call( "ComposeMultiTransform", as.character( c(...) ) ) ;
}
