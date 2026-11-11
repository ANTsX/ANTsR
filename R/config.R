#' Set ANTs deterministic behavior
#'
#' @name setANTsDeterministic
#' @rdname setANTsDeterministic
#' @param on Set deterministic behavior.
#' @param seedValue Assign random seed value. When \code{on = TRUE} and this
#'   is not \code{NULL}, it is used for \code{set.seed()}, stored in
#'   \code{options(ants.randomSeed)} and \code{ANTS_RANDOM_SEED}, and passed
#'   to ANTs as \code{--random-seed} by \code{antsRegistration} and
#'   \code{labelImageRegistration}.
#' @details \code{on = FALSE} clears the stored seed so later registrations
#'   are no longer seeded.
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
  } else {
    options( ants.randomSeed = NULL )
    Sys.unsetenv( "ANTS_RANDOM_SEED" )
  }
  invisible()
}
