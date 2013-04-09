antsCopyImageInfo <- function( reference, target )
{
  if ( !(class(target) == "antsImage") || !(class(target) == "antsImage") ) {
    stop( "Both inputs must be of class 'antsImage'")
  }  
  
  antsSetOrigin( target, as.numeric(antsGetOrigin(reference)) )
  antsSetDirection( target, antsGetDirection(reference) )
  antsSetSpacing( target, as.numeric(antsGetSpacing(reference)) )
  return (target)
}