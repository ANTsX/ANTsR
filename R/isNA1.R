# TRUE only for a single atomic NA: the "not supplied" default used by many
# arguments. Vectors, matrices, data frames, lists and images all count as
# supplied, and this never errors (unlike if (is.na(x)) on such inputs,
# which errors on R >= 4.2 and silently used only the first element before).
.isNA1 <- function(x) {
  is.atomic(x) && length(x) == 1L && is.na(x)
}

# Stop with a clear message if a supplied argument contains NA. A vector of
# NAs passes .isNA1() (it is not the single-NA default), and the NAs then
# flowed into the computation: silently NA/empty results or obscure errors.
.stopIfNA <- function(x, name) {
  if (anyNA(x)) {
    stop(name, " must not contain NA", call. = FALSE)
  }
  invisible(x)
}

# Validate preprocessfMRI()/preprocessRestingBOLD() smoothing options up
# front: they were only checked at the very end, after motion correction,
# CompCor and filtering had run. Messages match the original checks.
.checkSpatialSmoothing <- function(type, parameters) {
  if (!is.character(type) || length(type) != 1 ||
    !type %in% c("none", "gaussian", "perona-malik")) {
    stop("Unrecognized smoothing option.")
  }
  if (type == "gaussian" && length(parameters) != 1) {
    stop("Expecting a single scalar parameter.")
  }
  if (type == "perona-malik" && length(parameters) != 2) {
    stop("Expecting a two element vector.")
  }
  invisible(TRUE)
}
