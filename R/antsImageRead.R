antsImageRead <- function( filename , pixeltype , dimension  ){
	.Call( "antsImageRead", filename , pixeltype , dimension ) ;
}
