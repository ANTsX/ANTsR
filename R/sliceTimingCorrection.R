sliceTimingCorrection <- function( fmri, sliceTime=NA )

{
tr <- antsGetSpacing(fmri)[length(dim(fmri))]

if ( is.na(sliceTime) ) 
{
  sliceTime <- tr / ( dim(fmri)[length(dim(fmri))-1] )
  print( paste( "Assuming a slice timing of", sliceTime ) )
}

corrected <- antsImageClone(fmri)
ImageMath( 4, corrected, "SliceTimingCorrection", fmri, sliceTime, "sinc" )
return( corrected )
}
