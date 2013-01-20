antsApplyTransforms <- function( fixed = NA, moving = NA, transformlist="",interpolator="Linear", ... ){
  numargs<-nargs()
  if (  missing(fixed) | missing(moving) | missing( transformlist ) )
    {
    cat(" warpedimg<-antsApplyTransforms( fixed=img1 , moving=img2 , transformlist=list(\"my0GenericAffine.mat\",\"my1Warp.nii.gz\") ) ")
    cat("\n\n")
    cat("For full mode: use standard ants call as in antsRegistration full mode ... \n\n")
    cat(" antsApplyTransforms(\"-d\",\"2\",\"-i\",\"r64slice.nii.gz\",\"-o\",\"temp.nii.gz\",\"-r\",\"r16slice.nii.gz\",\"-t\",\"./Z0GenericAffine.mat\") \n")
    cat("for full help: \n")
    cat("use .Call( \"antsApplyTransforms\", int_antsProcessArguments( c(list(\"--help\")) ) );
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
        fixed<-antsImageClone(fixed,"double")
        moving<-antsImageClone(moving,"double")
        warpedmovout<-antsImageClone(fixed);
        f<-fixed
        m<-moving
        wmo<-warpedmovout
        mytx<-list()
        for ( i in c(1:length(transformlist)) ) 
          {
          mytx<-list( mytx,"-t",transformlist[ i ])
          }
        args<-list( d=fixed@dimension, i=m, o=wmo, r=f, n=interpolator,unlist( mytx ))
        .Call( "antsApplyTransforms", int_antsProcessArguments( c(args) ) ) ;
        return( antsImageClone(warpedmovout,inpixeltype) )
        }
      if ( ! ttexists  ) cat("Problem in arg list \n see usage by calling antsApplyTransforms() w/o arguments \n")
    }
    return(0)
  }
  .Call( "antsApplyTransforms", int_antsProcessArguments( c(args) ) ) ;
}
