lappend <- function(lst, obj) {
  if ( typeof( obj ) != "list" )
    {
    lst[[length(lst)+1]] <- obj
    return(lst)
    }
  if ( typeof(obj) == "list" & typeof( lst ) != "list" )
    {
    lstout<-list( lst )
    for ( i in 1:length(obj) )
      {
      lstout[[length(lstout)+1]] <- obj[[i]]
      }
    return(lstout)
    }
  if ( typeof(obj) == "list" & typeof( lst ) == "list" )
    {
    for ( i in 1:length(obj) )
      {
      lst[[length(lst)+1]] <- obj[[i]]
      }
    return(lst)
    }
  else return(NA)
}


