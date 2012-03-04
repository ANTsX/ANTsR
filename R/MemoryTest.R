MemoryTest <- function(...){
	.Call( "MemoryTest", c(...) , PACKAGE = "ANTsR" ) ;
}
