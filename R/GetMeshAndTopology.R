GetMeshAndTopology <- function(...){
	.Call( "GetMeshAndTopology", c(...) , PACKAGE = "ANTsR" ) ;
}

