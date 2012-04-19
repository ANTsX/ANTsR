CreateDTICohort <- function(...){
	.Call( "CreateDTICohort", as.character( c(...) ) ) ;
}
