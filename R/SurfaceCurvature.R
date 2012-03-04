SurfaceCurvature <- function(...){
	.Call( "SurfaceCurvature", c(...) , PACKAGE = "ANTsR" ) ;
}
