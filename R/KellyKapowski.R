KellyKapowski <- function( kkargs=NA )
{
  if ( typeof( kkargs ) != "list" | is.na(kkargs) )
    {
    print("Input error - should list params")
    return(NULL)
    }
  for ( i in c(1:length(kkargs)) )
    {
      myentry<-kkargs[[i]]
      if ( names(kkargs)[i] == "s" & class( myentry )[1] == "antsImage" )
        {
#        kkargs[[i]]<-antsImageClone( kkargs[[i]] , "unsigned int" )
        }
    }
  .Call( "KellyKapowski", int_antsProcessArguments( c(kkargs) ) ) ;
}
