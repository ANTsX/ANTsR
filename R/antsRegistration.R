antsRegistration <- function(...){
	.Call( "antsRegistration", c(...) , PACKAGE = "ANTsR" ) ;
}

