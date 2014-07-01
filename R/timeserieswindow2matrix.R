timeserieswindow2matrix <- function( timeseriesmatrix , mask, eventlist, timewindow, zeropadvalue=0, spacing=NA )
{
  if ( length(dim(timeseriesmatrix)) != 2 )
    {
    print("Mask should be of dimensionality 3")
    return(NA)
    }
  if ( length(dim(mask)) != 3 )
    {
    print("ERROR: Mask should be of dimensionality 3")
    return(NA)
    }
# first create the new mask
  mask4d <- makeImage( c(dim(mask),(timewindow+zeropadvalue*2) ) , 0 )
  mask4d <- as.array( mask4d )
  for ( i in (zeropadvalue+1):(zeropadvalue+timewindow) )
    {
    maskvec<-mask[ mask >= 0 ]
    mask4d[,,,i]<-maskvec
    }
  mask4d<-as.antsImage( mask4d )
  if ( ! is.na( spacing ) )
    if ( length(spacing) == 4 ) 
      antsSetSpacing( mask4d, spacing )
  nvox4dmask<-sum( mask4d == 1 )
  if ( ncol(timeseriesmatrix)*timewindow !=  nvox4dmask ) {
    print("ERROR: ncol(timeseriesmatrix)*timewindow !=  nvox4dmask")
    return(mask4d)
  }
  nevents<-length(eventlist)
  if ( min(eventlist) < 0 ) {
    print("ERROR: min(eventlist) < 0 ")
    return(mask4d)
  }
  if ( max(eventlist) > nrow(timeseriesmatrix) ) {
    print("ERROR: max(eventlist) > nrow(timeseriesmatrix) ")
    return(mask4d)
  }
  outmat<-matrix( rep(0, nvox4dmask*nevents) , nrow = nevents )
  ct<-1
  for ( i in eventlist ) {
    maxrow<-i+(timewindow-1)
    if ( maxrow > nrow(timeseriesmatrix) ) maxrow<-nrow(timeseriesmatrix)
    locmat<-as.numeric( t( timeseriesmatrix[ i:maxrow, ] ) )
    if ( length(locmat) == nvox4dmask ) outmat[ct,]<-locmat 
    ct<-ct+1
  }
  return( list(eventmatrix=outmat, mask4d=mask4d) )
} 
