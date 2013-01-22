antsMotionCorr <- function(...){
  .Call( "antsMotionCorr", int_antsProcessArguments( c(...) ) ) ;
  gc() # trigger garbage collection
}
