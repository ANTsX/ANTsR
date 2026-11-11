# TRUE only for a single atomic NA: the "not supplied" default used by many
# arguments. Vectors, matrices, data frames, lists and images all count as
# supplied, and this never errors (unlike if (is.na(x)) on such inputs,
# which errors on R >= 4.2 and silently used only the first element before).
.isNA1 <- function(x) {
  is.atomic(x) && length(x) == 1L && is.na(x)
}
