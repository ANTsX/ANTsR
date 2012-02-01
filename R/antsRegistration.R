# function wrapper to read an image file
antsRegistration <- function(...){
	.Call( "antsRegistration", c(...) , PACKAGE = "Ritk" ) ;
}

