antsMotionCorr <- function(...){
  .Call( "antsMotionCorr", int_antsProcessArguments( c(...) ) , PACKAGE="libRantsMotionCorr" ) ;
  gc() # trigger garbage collection
}
