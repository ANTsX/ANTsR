subjectDataToGroupDataFrame <- function( csvlist, usecol, mycolname=NA, datarownames=NA ) {
  if (nargs() == 0) {
    print("Usage:  x_subjectDataToGroupDataFrameed<-subjectDataToGroupDataFrame( x, 1 ) ")
    return(1)
  }
  data1<-read.csv( csvlist[1] )
  if ( ! is.numeric(usecol) ) mycol<-which( colnames(data1) == usecol ) else mycol<-usecol
  if ( is.na(mycol) ) { print(paste("poorly chosen column name",mycol)); return; }
  if ( is.na( mycolname ) ) mycolname<-colnames(data1)[mycol]
  ncl<-nrow(data1)
  if ( ! is.na( datarownames ) ) rownames(data1) <- datarownames
  mycolnames<-paste(mycolname,rownames(data1),sep='')
  mat<-matrix( rep(NA,ncl*length(csvlist) ) , nrow=length(csvlist) )
  for ( i in 1:length(csvlist) )
    {
    data1<-read.csv( csvlist[i] )
    mat[i,]<-data1[,mycol]
    }
  mydf<-data.frame( mat )
  colnames( mydf )<-mycolnames
  rownames( mydf )<-csvlist
  return( mydf )
} 
