convertRd2rst<- function( infile , outfile="")
{
  library(tools)
  myusage<-"usage: convertRd2rst('mymanfile.Rd')"
  if ( ! file.exists( infile ) )
    {
    print( paste("file", infile, "does not exist."))
    print(myusage)
    return(NULL)
    }
  rstheader<-"=========================\n"
  Rd <- parse_Rd(infile)
  tags <- tools:::RdTags(Rd)
  myfn<-Rd[[which(tags == "\\name")]][[1]]
  myfn<-paste(myfn,"\n")
  cat(rstheader,file=outfile,append = FALSE)
  cat(myfn,file=outfile,append = TRUE)
  cat(rstheader,file=outfile,append = TRUE)
  cat("\n",file=outfile,append = TRUE)
  mytitle<-Rd[[which(tags == "\\title")]][[2]][1]
  mytitle<-paste("purpose: ","\n\n",mytitle,"\n")
  cat(mytitle,file=outfile,append = TRUE)
  dlength<-length( Rd[[which(tags == "\\description")]] )
  if ( dlength == 2 )
    {
    mydescription<-Rd[[which(tags == "\\description")]][[2]][1]
    mydescription<-paste("description: ","\n\n",mydescription,"\n")
    cat(mydescription,file=outfile,append = TRUE)
    }
  if ( dlength > 2 )
    {
    basedescription<-Rd[[which(tags == "\\description")]]
    mydescription<-basedescription[[2]][1]
    for ( i in c(3:(dlength)) ) {
      mydescription<-paste(mydescription,basedescription[[i]][[1]][1])
    }
    mydescription<-paste("description: ","\n\n",mydescription,"\n\n")
    cat(mydescription,file=outfile,append = TRUE)
    }
  baseusage<-Rd[[which(tags == "\\usage")]]
  dlength<-length( baseusage )
  myusage<-baseusage[[2]][1]
  for ( i in c(3:(dlength)) ) {
    myusage<-paste(myusage,baseusage[[i]][[1]][1])
  }
  print( baseusage )
  myusage<-paste("usage: ","\n\n",myusage,"\n")
  cat(myusage,file=outfile,append = TRUE)
  # FIXME - need to parse arguments and loop over all 
#  myarguments<-Rd[[which(tags == "\\arguments")]][[2]][1]
#  myarguments<-paste("arguments: ",myarguments)
#  cat(myarguments,file=outfile,append = TRUE)
  cat("\n",file=outfile,append = TRUE)
}
