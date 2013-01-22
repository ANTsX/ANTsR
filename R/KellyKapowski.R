KellyKapowski <- function(  d=NA, s=NA, g=NA, w=NA, c="[45,0.0,10]", r=0.025, m=1.5, outimg=outimg, ... )
{
  if ( missing( d ) | missing( s ) | missing( g ) | missing( w ) | missing( c ) | missing( r ) | missing( m ) | missing( outimg ) )
    {
    print("Input error - check params & usage")
    return(NULL)
    }
  if ( class( s )[1] == "antsImage" )
    {
    s<-antsImageClone( s , "unsigned int" )
    }
#               KellyKapowski( d=3, s=simg, g=gimg,w=wimg,c=10,r=0.5,m=1,o=oimg )
  kkargs<-list( d=d, s=s, g=g, w=w, c=10, r=0.5, m=1, outimg=outimg )
  .Call( "KellyKapowski", int_antsProcessArguments( c(kkargs) ) ) ;
}
