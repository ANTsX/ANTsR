int_antsProcessArguments <- function( args )
{
  if( typeof( args ) == "list" )
  {
    for( i in ( 1:length(args) ) )
    {
      args[[i]] = int_antsExtractXptrAsString( args[[i]] )
    }
  }
  return( as.character( args ) )
}
