splitData <- function(data.source, ratio, return.rows = FALSE){
  if (ratio > 1) {
    split.cv <- TRUE 
  } else { 
    split.cv <- FALSE
  }
  nsubjects <- dim(data.source)[1]
  if (split.cv){
    mylist <- list()
    fold.ids <- sample(nsubjects)%%round(ratio)+1
    for (i in sort(unique(fold.ids))){
      subjects.in <- (1:nsubjects)[fold.ids != i] 
      subjects.out <- (1:nsubjects)[fold.ids == i]
      data.in <- data.source[subjects.in, ]
      data.out <- data.source[subjects.out, ]
      mylist[[paste("fold", i, sep='')]] <- list("data.in"=data.in, "data.out"=data.out)
      if (return.rows) {
	mylist[[paste("fold", i, sep='')]] <- 
	  list("data.in"=data.in, "data.out"=data.out, "rows.in"=subjects.in, "rows.out"=subjects.out)
      }
    }
    return(mylist)
  } else if (!split.cv){
    subjects.in <- sort(sample(nsubjects, ratio * nsubjects, replace=FALSE))
    data.in <- data.source[subjects.in, ]
    data.out <- data.source[-subjects.in, ]
    mylist <- list("data.in"=data.in, "data.out"=data.out) 
    if (return.rows) mylist <- list("data.in"=data.in, "data.out"=data.out,
				    "rows.in"=subjects.in, "rows.out"=(1:nsubjects)[-subjects.in])
    return(mylist)
  }
}
