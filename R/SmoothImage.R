SmoothImage <- function(...){
  .Call( "SmoothImage", int_antsProcessArguments( c(...) ) ) ;
}
