#' Simple list append tool
#' 
#' Append an item to a list , creating a new list or combine two lists
#' 
#' 
#' @param inlist list to which you will append the 2nd item
#' @param myitem to be added to the list
#' @return outputs the appended list
#' @author Avants BB, the internet
#' @examples
#' 
#' \dontrun{
#' mat<-replicate(100, rnorm(20)) 
#' mat2<-replicate(100, rnorm(20))
#' mylist<-list(mat)
#' mylist<-lappend( mylist, mat2 ) 
#' }
#' 
#' @export lappend
lappend <- function(lst, obj) {
  if (typeof(obj) != "list") {
    lst[[length(lst) + 1]] <- obj
    return(lst)
  }
  if (typeof(obj) == "list" & typeof(lst) != "list") {
    lstout <- list(lst)
    for (i in 1:length(obj)) {
      lstout[[length(lstout) + 1]] <- obj[[i]]
    }
    return(lstout)
  }
  if (typeof(obj) == "list" & typeof(lst) == "list") {
    for (i in 1:length(obj)) {
      lst[[length(lst) + 1]] <- obj[[i]]
    }
    return(lst)
  } else return(NA)
} 
