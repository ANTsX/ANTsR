#' Processing arguments for command line parsing
#'
#' @rdname int_antsProcessArguments
#' @param args arguments to parse into those for \code{ANTsR} functions
#' @return A character vector
#' args = "hey"
#' .int_antsProcessArguments(args)
#' args = list(h = "hey")
#' .int_antsProcessArguments(args)
#' args = list(h = list("hey"))
#' .int_antsProcessArguments(args)
.int_antsProcessArguments <- function(args) {
  char_vect <- ""
  if (typeof(args) == "list") {
    char_vect <- NULL
    for (i in (1:length(args))) {
      if (length(names(args)) != 0) {
        if (nchar(names(args)[i]) > 1) {
          char_vect <- c(char_vect, paste("--", names(args)[i], sep = ""))
        } else {
          char_vect <- c(char_vect, paste("-", names(args)[i], sep = ""))
        }
      }
      if (typeof(args[[i]]) == "list") {
        char_vect <- c(char_vect, paste(args[[i]]$name, "[", sep = ""))
        args[[i]]$name <- NULL
        for (j in (1:length(args[[i]]))) {
          char_vect <- c(char_vect, as.character(.int_antsExtractXptrAsString(args[[i]][[j]])))
        }
        char_vect <- c(char_vect, "]")
      } else {
        char_vect <- c(char_vect, as.character(.int_antsExtractXptrAsString(args[[i]])))
      }
    }
  }
  return(char_vect)
}
