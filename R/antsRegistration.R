antsRegistration <- function( fixed = NA, moving = NA, typeofTransform="",outprefix="", ... ){
  numargs<-nargs()
  if ( numargs < 2 )
    {
    cat("for simplified mode: \n")
    cat(" antsRegistration( fixed , moving , typeofTransform = c(\"Rigid\",\"Affine\",\"SyN\"),  outputPrefix=\"./antsRegOut\" \n")
    cat("")
    cat("For full mode: use standard ants call , e.g. : \n")
    cat(" ANTsR::antsRegistration( \"-d\", \"2\", \"-m\", \"mi[r16slice.nii.gz,r64slice.nii.gz,1,20,Regular,0.05]\", \"-t\", \"affine[1.0]\", \"-c\", \"2100x1200x1200x0\", \"-s\", \"3x2x1x0\", \"-f\", \"4x3x2x1\",\"-u\", \"1\", \"-o\", \"[xtest,xtest.nii.gz,xtest_inv.nii.gz]\" ) \n")
    cat("full help: \n")
    .Call( "antsRegistration", int_antsProcessArguments( c( list("--help")) ) );
    return(0);
    }
  args <- list(fixed,moving,typeofTransform,outprefix,...)
  if ( ! is.character(fixed) ) {
    if ( fixed@class[[1]] == "antsImage" & moving@class[[1]] == "antsImage" )
      {
      ttexists <- FALSE
      if ( typeofTransform == "Rigid"  |
           typeofTransform == "Affine" |
           typeofTransform == "SyN"    )
        {
        ttexists <- TRUE
        }
      if ( ttexists  )
        {
        cat("use simple parameterization \n")
        ffn<-paste(outprefix,antsrmakeRandomString(),".nii.gz",sep='')
        mfn<-paste(outprefix,antsrmakeRandomString(),".nii.gz",sep='')
        antsImageWrite(fixed,ffn)
        antsImageWrite(moving,mfn) #
        if ( typeofTransform == "SyN"  ) {
        args<-list("-d",as.character(fixed@dimension),"-r",paste("[",ffn,",",mfn,",1]",sep=''),"-m",paste("mattes[",ffn,",",mfn,",1,32,regular,0.2]",sep=''),"-t","Affine[0.25]","-c","2100x1200x1200x0","-s","3x2x1x0","-f", "4x3x2x1" ,"-m",paste("mattes[",ffn,",",mfn,",1,32]",sep=''),"-t",paste(typeofTransform,"[0.25,3,0]",sep=''),"-c","2100x1200x1200x0","-s","3x2x1x0","-f", "4x3x2x1" ,"-u","1","-z","1","-o", paste("[",outprefix,",",outprefix,".nii.gz,",outprefix,"inv.nii.gz]",sep=''))
        }
        if ( typeofTransform == "Rigid" | typeofTransform == "Affine" ) {
        args<-list("-d",as.character(fixed@dimension),"-r",paste("[",ffn,",",mfn,",1]",sep=''),"-m",paste("mattes[",ffn,",",mfn,",1,32,regular,0.2]",sep=''),"-t",paste(typeofTransform,"[0.25]",sep=''),"-c","2100x1200x1200x0","-s","3x2x1x0","-f", "4x3x2x1" ,"-u","1","-z","1","-o", paste("[",outprefix,",",outprefix,".nii.gz,",outprefix,"inv.nii.gz]",sep=''))
        }
        .Call( "antsRegistration", int_antsProcessArguments( c(args) ) ) ;
        unlink(ffn) 
        unlink(mfn) 
        return(0)
        }
      if ( ! ttexists  ) cat("Problem in arg list \n see usage by calling antsRegistration() w/o arguments \n")
    }
    return(0)
    }
  .Call( "antsRegistration", int_antsProcessArguments( c(args) ) ) ;
}

###############################################################
#
# antsrmakeRandomString(n, length)
# function generates a random string random string of the
# length (length), made up of numbers, small and capital letters
# helper function
antsrmakeRandomString <- function(n=1, mylength=12)
{
    randomString <- c(1:n)                  # initialize vector
    for (i in 1:n)
    {
        randomString[i] <- paste(sample(c(0:9, letters, LETTERS),
                                 mylength, replace=TRUE),
                                 collapse="")
    }
    return(randomString)
}
