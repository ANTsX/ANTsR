convertRd2rst <- function(infile, outfile = "") {
  usePkg("tools")
  myusage <- "usage: convertRd2rst('mymanfile.Rd')"
  if (!file.exists(infile)) {
    print(paste("file", infile, "does not exist."))
    print(myusage)
    return(NULL)
  }
  rstheader <- "=========================\n"
  print(paste("Process:", infile))
  Rd <- parse_Rd(infile)
  tags <- tools:::RdTags(Rd)
  myfnr <- Rd[[which(tags == "\\name")]][[1]]
  myfn <- paste(myfnr, "\n")
  cat(rstheader, file = outfile, append = FALSE)
  cat(myfn, file = outfile, append = TRUE)
  cat(rstheader, file = outfile, append = TRUE)
  cat("\n", file = outfile, append = TRUE)
  mytitle <- Rd[[which(tags == "\\title")]][[2]][1]
  mytitle <- paste("purpose: ", "\n\n", mytitle, "\n")
  cat(mytitle, file = outfile, append = TRUE)

  basedescription <- Rd[[which(tags == "\\description")]]
  dlength <- length(basedescription)
  mydescription <- basedescription[[2]][1]
  if (dlength > 2) {
    for (i in c(3:(dlength))) {
      mydescription <- paste(mydescription, basedescription[[i]][[1]][1])
    }
  }
  mydescription <- paste("description: ", "\n\n", mydescription, "\n\n")
  cat(mydescription, file = outfile, append = TRUE)

  baseusage <- Rd[[which(tags == "\\usage")]]
  dlength <- length(baseusage)
  myusage <- baseusage[[2]][1]
  if (dlength > 2) {
    for (i in c(3:(dlength))) {
      myusage <- paste(myusage, "\n", baseusage[[i]][[1]][1])
    }
  }
  myusage <- paste("usage: ", "\n\n", myusage, "\n")
  cat(myusage, file = outfile, append = TRUE)

  baseexamples <- Rd[[which(tags == "\\examples")]]
  myrrsthd <- paste(".. {r ", myfnr, ",eval=FALSE,warning=FALSE,results=\"hide\",message=FALSE,echo=TRUE,results=\"hide\"} \n ")

  dlength <- length(baseexamples[[2]])
  myexamples <- baseexamples[[2]][[1]][1]
  if (dlength > 1) {
    for (i in c(2:(dlength))) {
      myexamples <- paste(myexamples, "\n", baseexamples[[2]][[i]][1])
    }
  }
  myexamples <- paste("examples: \n", myrrsthd, "\n", myexamples, "\n")
  cat(myexamples, file = outfile, append = TRUE)
  cat(".. ..\n", file = outfile, append = TRUE)
  # FIXME - need to parse arguments and loop over all myarguments<-Rd[[which(tags
  # == '\\arguments')]][[2]][1] myarguments<-paste('arguments: ',myarguments)
  # cat(myarguments,file=outfile,append = TRUE)
  cat("\n", file = outfile, append = TRUE)
}
