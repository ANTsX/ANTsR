ResampleImageBySpacing <- function(...){
	.Call( "ResampleImageBySpacing", as.character( c(...) ) ) ;
}
