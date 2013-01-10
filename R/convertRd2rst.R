convertRd2rst<- function( infile )
{
  library(tools)
  myusage<-"usage: convertRd2rst('mymanfile.Rd')"
  if ( ! file.exists( infile ) )
    {
    print( paste("file", infile, "does not exist."))
    print(myusage)
    return(NULL)
    } 
  Rd <- parse_Rd(infile)
  tags <- tools:::RdTags(Rd)
  myfn<-Rd[[which(tags == "\\name")]][[1]]
  myfn<-paste("function: ",myfn,"\n")
  cat(myfn)
  cat("\n")
  mytitle<-Rd[[which(tags == "\\title")]][[2]][1]
  mytitle<-paste("purpose: ","\n\n",mytitle,"\n")
  cat(mytitle)
  mydescription<-Rd[[which(tags == "\\description")]][[2]][1]
  mydescription<-paste("description: ","\n\n",mydescription,"\n")
  cat(mydescription)
  myusage<-Rd[[which(tags == "\\usage")]][[2]][1]
  myusage<-paste("usage: ","\n\n",myusage,"\n")
  cat(myusage)
  # FIXME - need to parse arguments and loop over all 
#  myarguments<-Rd[[which(tags == "\\arguments")]][[2]][1]
#  myarguments<-paste("arguments: ",myarguments)
#  cat(myarguments)
  cat("\n")
}
