#' Simple subjectDataToGroupDataFrameing function.
#' 
#' SubjectDataToGroupDataFrames take a list of subject-level csv files of the
#' same type and converts them to a data frame with consistent column naming
#' for the values and where rows correspond to subjects.
#' 
#' 
#' @param csvlist input list of csv files eg by Sys.glob ...
#' @param usecol a column name or number e.g. 1 or 'Volume'
#' @param mycolname rename the column by this string (optional)
#' @param datarownames the desired now names (optional)
#' @return data frame is output
#' @author Avants BB
#' @examples
#' 
#' \dontrun{
#' data(aal,package='ANTsR')
#' csvlist<-Sys.glob('*md*csv')
#' mypopulationdataframe<-subjectDataToGroupDataFrame( csvlist , 'Mean' , datarownames=aal$label_name )
#' # should have each ROI value for each subject listed in a large data frame
#' }
#' 
#' @export subjectDataToGroupDataFrame
subjectDataToGroupDataFrame <- function(csvlist, usecol, mycolname = NA, datarownames = NA) {
  if (nargs() == 0) {
    print("Usage:  x_subjectDataToGroupDataFrameed<-subjectDataToGroupDataFrame( x, 1 ) ")
    return(1)
  }
  inds <- which(!is.na(csvlist))
  data1 <- read.csv(csvlist[inds[1]])
  if (!is.numeric(usecol)) 
    mycol <- which(colnames(data1) == usecol) else mycol <- usecol
  if (is.na(mycol)) {
    print(paste("poorly chosen column name", mycol))
    return
  }
  if (is.na(mycolname)) 
    mycolname <- colnames(data1)[mycol]
  ncl <- nrow(data1)
  if (length(datarownames) > 1) 
    if (length(datarownames) == length(rownames(data1))) 
      rownames(data1) <- datarownames
  mycolnames <- paste(mycolname, rownames(data1), sep = "")
  mat <- matrix(rep(NA, ncl * length(csvlist)), nrow = length(csvlist))
  for (i in inds) {
    data2 <- read.csv(csvlist[i])
    addto <- TRUE
    if (nrow(data2) != nrow(data1)) {
      print(paste("Warning---", csvlist[i], "has a different # of rows"))
      addto <- FALSE
    }
    if (ncol(data2) != ncol(data1)) {
      print(paste("Warning---", csvlist[i], "has a different # of cols"))
      addto <- FALSE
    }
    if (addto == TRUE) 
      mat[i, ] <- data2[, mycol]
  }
  mydf <- data.frame(mat)
  colnames(mydf) <- mycolnames
  if (length(unique(csvlist)) == nrow(mydf)) 
    rownames(mydf) <- csvlist
  return(mydf)
} 
