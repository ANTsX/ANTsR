ComposeMultiTransform <- function(...){
	.Call( "ComposeMultiTransform", c(...) , PACKAGE = "ANTsR" ) ;
}
