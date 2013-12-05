regressionNetworkViz <- function( mylm , sigthresh=0.05, whichviz="Sankey", outfile="temp.html", mygroup = 0 , logvals=TRUE, verbose=FALSE, correlateMyOutcomes = NA, corthresh = 0.9, zoom = F  ) {
  if (nargs() == 0) {
    return(1)
  }
  pckg <- try(require(d3Network))
  if (!pckg) {
    getPckg("d3Network")
  }
  library(d3Network)
  demognames<-rownames(mylm$beta.pval)
  jjnames<-c(demognames,colnames(mylm$beta.pval))
  if ( length( mygroup ) == 1 )
    mygroup<-c(rep(1,length(demognames)),rep(2,ncol(mylm$beta.pval)))
  JJNodes<-data.frame( name=jjnames, group=mygroup )
  jjsources<-c()
  jjtargets<-c()
  jjvalues<-c()
  for ( i in 1:nrow(mylm$beta.pval) )
    {
      if ( verbose ) {
        print(demognames[i])
        print(mylm$beta.pval[i,])
      }
      myselection<-which( mylm$beta.pval[i,] < sigthresh )
      if ( length( myselection ) > 0 )
        {
          jjsources<-c( jjsources, rep( i-1, length( myselection ) ) )
          jjtargets<-c( jjtargets, myselection-1+length(demognames) )
          if ( logvals )
            jjvalues<-c( jjvalues, abs( log ( mylm$beta.pval[i,myselection] ) ) )
          else 
            jjvalues<-c( jjvalues, ( 1 - mylm$beta.pval[i,myselection] ) * 10 )
        }
    }
  if ( ! is.na( correlateMyOutcomes ) )
    {
      if ( ncol(correlateMyOutcomes) == nrow(correlateMyOutcomes) )
      {
      mycor<-( correlateMyOutcomes )
      for ( i in 1:nrow( mycor ) )
        {
          myselection<-which( mycor[i,] > corthresh )
          myselection <- myselection[ myselection > i ] 
          if ( length( myselection ) > 0 )
            {
              jjsources<-c( jjsources, rep( i-1+length(demognames), length( myselection ) ) )
              jjtargets<-c( jjtargets, myselection-1+length(demognames) )
              if ( logvals )
                jjvalues<-c( jjvalues, abs( log ( 1 - mycor[i,myselection] ) ) )
              else 
                jjvalues<-c( jjvalues, ( mycor[i,myselection] ) * 10 )
            }
        }
      } # if it's a correlation matrix 
    }
  JJLinks<-data.frame(source=jjsources,target=jjtargets,value=jjvalues)
  if ( whichviz == "Sankey" ) {
    d3Sankey(Links = JJLinks, Nodes = JJNodes, Source = "source",
           Target = "target", Value = "value", NodeID = "name",
           fontsize = 12, nodeWidth = 30, width = 700,file=outfile)
  } else {
    d3ForceNetwork(Links = JJLinks, Nodes = JJNodes, 
               Source = "source", Target = "target", 
               Value = "value", NodeID = "name", 
               Group = "group", width = 550, height = 400, zoom=zoom ,
               opacity = 0.9,file=outfile)
  }
  return( list( mynodes=JJNodes, mylinks=JJLinks ) )
} 
