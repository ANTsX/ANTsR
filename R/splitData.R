#' Split data for testing and training
#'
#' Split data into testing and training sets for cross-validation.
#'
#'
#' @param data.source Data frame or matrix of input data to be split.
#' @param ratio If greater than one, number of folds to split data into.
#' Otherwise, proportion of rows in training data.  See \code{Value}.
#' @param return.rows If \code{TRUE}, row numbers of testing and training data
#' are also returned.
#' @return A list containing the input data, split into testing and training
#' sets.  If \code{ratio} is greater than one, returns a list with \code{ratio}
#' entries, each with 1/\code{ratio} of the input data in the testing set
#' (\code{data.out}) and the rest in the training set (\code{data.in}).
#' Otherwise, returns a list with one split of the data, with \code{ratio} of
#' the input data in the testing set and the rest in the training set.
#' @author Kandel BM and Avants B
#' @examples
#' \dontrun{
#' n <- 30
#' ratio <- 2 / 3
#' data.source <- data.frame(value = 1:n)
#' out <- splitData(data.source, ratio)
#' }
#'
#' @export splitData
splitData <- function(data.source, ratio, return.rows = FALSE) {
  if (ratio > 1) {
    split.cv <- TRUE
  } else {
    split.cv <- FALSE
  }
  nsubjects <- dim(data.source)[1]
  if (split.cv) {
    mylist <- list()
    fold.ids <- sample(nsubjects) %% round(ratio) + 1
    for (i in sort(unique(fold.ids))) {
      subjects.in <- (1:nsubjects)[fold.ids != i]
      subjects.out <- (1:nsubjects)[fold.ids == i]
      data.in <- data.source[subjects.in, ]
      data.out <- data.source[subjects.out, ]
      mylist[[paste("fold", i, sep = "")]] <- list(data.in = data.in, data.out = data.out)
      if (return.rows) {
        mylist[[paste("fold", i, sep = "")]] <- list(
          data.in = data.in, data.out = data.out,
          rows.in = subjects.in, rows.out = subjects.out
        )
      }
    }
    return(mylist)
  } else if (!split.cv) {
    subjects.in <- sort(sample(nsubjects, ratio * nsubjects, replace = FALSE))
    data.in <- data.source[subjects.in, ]
    data.out <- data.source[-subjects.in, ]
    mylist <- list(data.in = data.in, data.out = data.out)
    if (return.rows) {
      mylist <- list(
        data.in = data.in, data.out = data.out, rows.in = subjects.in,
        rows.out = (1:nsubjects)[-subjects.in]
      )
    }
    return(mylist)
  }
}
