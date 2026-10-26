#' Set ANTs deterministic behavior
#'
#' @name setANTsDeterministic
#' @rdname setANTsDeterministic
#' @param on Set deterministic behavior.
#' @param seedValue Assign random seed value.
#' @return No return value, called for side effects.
#' @examples
#' setANTsDeterministic(TRUE)
#' @export
setANTsDeterministic <- function( on = TRUE, seedValue = 123 ) 
{
  options( ants.deterministic = on )
  if( on ) {
    Sys.setenv( ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS = "1" )
    if ( !is.null( seedValue ) ) {
      set.seed( seedValue )
      options( ants.randomSeed = seedValue )
      Sys.setenv( ANTS_RANDOM_SEED = seedValue )
    }
  }
}
