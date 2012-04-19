GetMeshAndTopology <- function(...){
	.Call( "GetMeshAndTopology", as.character( c(...) ) ) ;
}

