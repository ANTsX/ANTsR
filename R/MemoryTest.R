MemoryTest <- function(...){
	.Call( "MemoryTest", c(...) , PACKAGE = "Ritk" ) ;
}
