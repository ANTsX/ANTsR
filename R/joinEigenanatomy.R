joinEigenanatomy <- function( datamatrix, mask, list_of_eanat_images , graphdensity = 0.65  )
  {
  if ( nargs() == 0 )
    {
    print("Usage: ")
    print( args( joinEigenanatomy ) )
    return(1)
    }
  pckg = try(require(igraph))
  if(!pckg) {
    getPckg("igraph")
  }
  library(igraph)
  decom2<-imageListToMatrix( list_of_eanat_images, mask)
  myproj<-datamatrix %*% t( decom2 )
  gg<-makeGraph( cor( myproj ), graphdensity ) 
  communitymembership<-gg$walktrapcomm$membership
  newelist<-list()
  for ( cl in 1:max( communitymembership ) )
    {
      newe<-antsImageClone( mydecom$eigenanatomyimages[[1]] )
      newe[ mask > 0 ]<-0 
      templist<-mydecom$eigenanatomyimages[ communitymembership == cl   ]
      for ( eimg in templist ) {   
        newe[ mask > 0 ] <- newe[ mask > 0 ] + eimg[ mask > 0 ] / sum( eimg[ mask > 0 ] ) 
        print(  sum( newe > 0 )/ sum( mask > 0 ) )
      }
      newe[ mask > 0 ] <- newe[ mask > 0 ] / sum( newe[ mask > 0 ] )
      newelist<-lappend( newelist, newe )
    }
  decom2<-imageListToMatrix( newelist, mask)
  myproj<-datamatrix %*% t( decom2 )
  colnames(myproj)<-paste("V",1:ncol(myproj),sep='')
  return( list( fusedlist=newelist, fusedproj = myproj, memberships = communitymembership ) )
}
