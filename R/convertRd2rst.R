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

  basedescription<-Rd[[which(tags == "\\description")]]
  dlength<-length( basedescription )
  mydescription<-basedescription[[2]][1]
  for ( i in c(3:(dlength)) ) {
    mydescription<-paste(mydescription,basedescription[[i]][[1]][1])
  }
  mydescription<-paste("description: ","\n\n",mydescription,"\n\n")
  cat(mydescription,file=outfile,append = TRUE)
  
  baseusage<-Rd[[which(tags == "\\usage")]]
  dlength<-length( baseusage )
  myusage<-baseusage[[2]][1]
  for ( i in c(3:(dlength)) ) {
    myusage<-paste(myusage,baseusage[[i]][[1]][1])
  }
  myusage<-paste("usage: ","\n\n",myusage,"\n")
  cat(myusage,file=outfile,append = TRUE)

  baseexamples<-Rd[[which(tags == "\\examples")]]
  dlength<-length( baseexamples[[2]] )
  myexamples<-baseexamples[[2]][[1]][1]
  for ( i in c(2:(dlength)) ) {
    myexamples<-paste(myexamples,baseexamples[[2]][[i]][1])
  }
  myexamples<-paste("examples: ","\n",myexamples,"\n")
  cat(myexamples,file=outfile,append = TRUE)

  # FIXME - need to parse arguments and loop over all 
#  myarguments<-Rd[[which(tags == "\\arguments")]][[2]][1]
#  myarguments<-paste("arguments: ",myarguments)
#  cat(myarguments,file=outfile,append = TRUE)
  cat("\n",file=outfile,append = TRUE)
}
