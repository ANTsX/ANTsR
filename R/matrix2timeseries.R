matrix2timeseries <- function(img, mask, mat) {
  if ( length(dim(img)) != 4 )
      {
      print("This function is for 4D images")
      return(img)
      }
  logmask <- (mask == 1)
  newimga<-as.array( img )
  newimga<-newimga*0
  for ( i in 1:dim(img)[4] ) newimga[,,,i][logmask]<-boldmat[i,]
  newimg<-as.antsImage(newimga)
  antsCopyImageInfo( img, newimg )
  return( newimg )
} 
