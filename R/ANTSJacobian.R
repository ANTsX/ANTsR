ANTSJacobian <- function(...){
	.Call( "ANTSJacobian", as.character( c(...) ) ) ;
}
