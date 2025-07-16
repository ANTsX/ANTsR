
# Set default options when package is loaded
.onLoad <- function( libname, pkgname ) 
{
  op <- options()
  op.ants <- list(
    ants.deterministic = FALSE,
    ants.randomSeed = NULL
  )
  toSet <- !( names( op.ants ) %in% names( op ) )
  if( any( toSet ) ) {
    options( op.ants[toSet] )
  } 
  invisible()
}

#' Set ANTs deterministic behavior
#'
#' @param on Set deterministic behavior.
#' @param seedValue Assign random seed value.
#'
#' @export setANTsDeterministic
setANTsDeterministic <- function( on = TRUE, seedValue = 123 ) 
{
  options( ants.deterministic = on )
  if( on ) {
    Sys.setenv( ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS = "1" )
    if ( !is.null( seedValue ) ) 
      {
      set.seed( seedValue )
      options( ants.randomSeed = seedValue )
      Sys.setenv( ANTS_RANDOM_SEED = seedValue )
      }
    }
}
