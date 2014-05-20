subjectDataToGroupDataFrame <- function( csvlist, usecol, mycolname=NA ) {
  if (nargs() == 0) {
    print("Usage:  x_subjectDataToGroupDataFrameed<-subjectDataToGroupDataFrame( x, 1 ) ")
    return(1)
  }
  data1<-read.csv( csvlist[1] )
  if ( is.na( mycolname ) ) mycolname<-colnames(data1)[usecol]
  ncl<-nrow(data1)
  mycolnames<-paste(mycolname,rownames(ff),sep='')
  mat<-matrix( rep(NA,ncl*length(csvlist) ) , nrow=length(csvlist) )
  for ( i in 1:length(csvlist) )
    {
    data1<-read.csv( csvlist[i] )
    mat[i,]<-data1[,usecol]
    }
  mydf<-data.frame( mat )
  colnames( mydf )<-mycolnames
  rownames( mydf )<-csvlist
  return( mydf )
} 
