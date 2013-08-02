antsApplyTransforms <- function( fixed = NA, moving = NA, transformlist="",interpolator="Linear", ... ){
  numargs<-nargs()
  if ( typeof( fixed ) == "list" )
    {
    .Call("antsApplyTransforms", int_antsProcessArguments( c(fixed,"-z",1,"--float",0) ) , PACKAGE="libRantsApplyTransforms") ;
    return(0);
    }
  if (  missing(fixed) | missing(moving) | missing( transformlist ) )
    {
    cat(" warpedimg<-antsApplyTransforms( fixed=img1 , moving=img2 , transformlist=list(\"my0GenericAffine.mat\",\"my1Warp.nii.gz\") ) ")
    cat("\n\n")
    cat("For full mode: use standard ants call as in antsApplyTransforms full mode ... \n\n")
    cat(" antsApplyTransforms(\"-d\",\"2\",\"-i\",\"r64slice.nii.gz\",\"-o\",\"temp.nii.gz\",\"-r\",\"r16slice.nii.gz\",\"-t\",\"./Z0GenericAffine.mat\") \n")
    cat("for full help: \n")
    cat("use .Call( \"antsApplyTransforms\", int_antsProcessArguments( c(list(\"--help\")) ), PACKAGE=\"libRantsApplyTransforms\" );
\n");
    return(0);
    }
  args <- list(fixed,moving,transformlist,interpolator,...)
  if ( ! is.character(fixed) ) {
    if ( fixed@class[[1]] == "antsImage" & moving@class[[1]] == "antsImage" )
      {
      ttexists <- TRUE
      for ( i in 1:length(transformlist) )
        {
        if ( !file.exists(transformlist[i]) ) ttexists <- FALSE
        }
      if ( ttexists  )
        {
        inpixeltype<-fixed@pixeltype
        fixed<-antsImageClone(fixed,'double')
        moving<-antsImageClone(moving,'double')
        warpedmovout<-antsImageClone(fixed,'double');
        f<-fixed
        m<-moving
        wmo<-warpedmovout
        mytx<-list()
        for ( i in c(1:length(transformlist)) ) 
          {
          ismat<-FALSE
          if ( i == 1 & length(transformlist) > 1 ) {
            if ( length( grep( ".mat",transformlist[ i ])) == 1 )
              {
              ismat <- TRUE
              print("treating this as an inverse transform")
              }
          }
          if ( !ismat ) mytx<-list( mytx,"-t",transformlist[ i ]) else mytx<-list( mytx,"-t",paste("[",transformlist[ i ],",1]",sep='') )
          }
        args<-list( d=fixed@dimension, i=m, o=wmo, r=f, n=interpolator, unlist( mytx ))
	myargs<-int_antsProcessArguments( c(args) )
	for ( jj in c(1:length(myargs)) )
	{
	if ( !is.na( myargs[jj] ) ) {
        if ( myargs[jj] == "-" )
	{
	myargs2<-rep(NA,(length(myargs)-1))
	myargs2[1:(jj-1)]<-myargs[1:(jj-1)]
	myargs2[jj:(length(myargs)-1)]<-myargs[(jj+1):(length(myargs))]
	myargs<-myargs2
        }
        } }
        .Call("antsApplyTransforms", c(myargs,"-z",1,"--float",0) , PACKAGE="libRantsApplyTransforms")
        gc()
        return( antsImageClone(warpedmovout,inpixeltype) )
        }
      if ( ! ttexists  ) cat("Problem in arg list \n see usage by calling antsApplyTransforms() w/o arguments \n")
    }
    return(0)
  }
#  if ( Sys.info()["sysname"] == "XXX" )
#    {
#    mycmd<-antsrParseListToString(  c(args) )
#    system( paste("antsApplyTransforms ", mycmd$mystr ) )
#    return( antsImageRead( mycmd$outimg, as.numeric(mycmd$outdim) ) )
#    }
  .Call("antsApplyTransforms", int_antsProcessArguments( c(args,"-z",1,"--float",0) ), PACKAGE="libRantsApplyTransforms" ) ;
  gc() # trigger garbage collection
}


antsrParseListToString  <- function( mylist , outimg=NA, outdim=NA )
  {
  mystr<-""
  len<-length(mylist)
  outimg<-""
  outdim<-11
  for ( x in 1:len )
    {
    if ( class( mylist[[x]] )[1] == "antsImage" )
      {
      tfn<-paste(tempdir(),'img',x,'.nii.gz',sep='')
      antsImageWrite( mylist[[x]] , tfn )
      mystr<-paste( mystr, tfn )
      outdim<-mylist[[x]]@dimension
      if ( typeof( mylist[[x-1]] ) == "character" )
        {
        if (  mylist[[x-1]] == "-o" ) outimg<-tfn
        }
      if ( typeof( mylist[[x-1]] ) != "S4" ) 
        if ( mylist[[x-1]] == "-o" ) outimg<-tfn 
      } else mystr<-paste( mystr, toString(mylist[[x]]) )
    }
  mystr<-sub(',',' ',mystr)
  mystr<-sub(' - ',' ',mystr)
  mystr<-sub('-t,','-t ',mystr)
  mystr<-sub(', ',' ',mystr)
  return( list(mystr=mystr, outimg=outimg, outdim=outdim ) ) 
  }

antsrParseListToString2  <- function( mylist , outimg=NA, outdim=NA )
  {
  mystr<-""
  outimg<-""
  outdim<-11
  len<-length(mylist)
  for ( x in 1:len )
    {
    mystr<-paste(mystr," -",names(mylist)[x]," ",sep='')
    if ( class( mylist[[x]] )[1] == "antsImage" )
      {
      tfn<-paste(tempdir(),'img',x,'.nii.gz',sep='')
      antsImageWrite( mylist[[x]] , tfn )
      mystr<-paste( mystr, tfn )
      outdim<-mylist[[x]]@dimension
      if ( names(mylist)[x] == "o" ) outimg<-tfn 
      } else mystr<-paste( mystr, toString(mylist[[x]]) )
    }
  mystr<-sub(',',' ',mystr)
  mystr<-sub(' - ',' ',mystr)
  mystr<-sub('-t,','-t ',mystr)
  mystr<-sub(', ',' ',mystr)
  return( list(mystr=mystr, outimg=outimg, outdim=outdim ) ) 
  }
