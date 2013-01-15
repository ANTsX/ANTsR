antsRegistration <- function( ... ){
  numargs<-nargs()
  if ( numargs < 2 )
    {
    .Call( "antsRegistration", int_antsProcessArguments( c( list("--help")) ) );
    return(0);
    }
    args <- list(...)
   .Call( "antsRegistration", int_antsProcessArguments( c(args) ) ) ;
#   args <- list(...)
#   newargs<-list( args[[1]] , args[[4]], "G", args[[2]], args[[3]])
#  .Call( "antsRegistration", int_antsProcessArguments( c(newargs) ) ) ;
}
