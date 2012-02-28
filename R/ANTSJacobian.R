ANTSJacobian <- function(...){
	.Call( "ANTSJacobian", c(...) , PACKAGE = "Ritk" ) ;
}
