antsImageWrite <- function( image , filename , pixeltype , dimension ){
	.Call( "antsImageWrite", image , filename , pixeltype , dimension ) ;
}
