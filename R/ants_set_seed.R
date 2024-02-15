#' Set seed for ANTsR functionality
#'
#' @param seed Seed to set to environment variable
#' \code{ANTS_RANDOM_SEED}
#'
#' @return Logical indicating the variable was set
#' @export
#'
#' @examples
#' ants.set.seed(1)
#' ants.set.seed("")
ants.set.seed = function(seed) {
  Sys.setenv(ANTS_RANDOM_SEED = seed)    
}
