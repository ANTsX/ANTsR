
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
