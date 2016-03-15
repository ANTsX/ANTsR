#' Render a function onto a surface.
#'
#' Use a system call to \code{antsSurf} in order to render a function onto
#' a surface.  This requires \code{antsSurf} to be in the enviornment as well
#' as \code{ConvertScalarImageToRGB}, both available in Advanced Normalization
#' Tools software on github.
#'
#' @param x input antsImage defining the surface on which to render
#' @param y input antsImage list defining the function to render
#' on the surface. these image(s) should be in the same space as \code{surf}.
#' @param quantlimits lower and upper quantile limits for overlay
#' @param colormap character, one of: grey, red, green, blue, copper, jet, hsv,
#' spring, summer, autumn, winter, hot, cool, overunder, custom
#' @param inflationFactor number of inflation iterations to run
#' @param rotationParams 3 Rotation angles expressed in degrees
#' @param verbose prints the command used to call \code{antsSurf}
#' @return no output
#' @author Avants BB, Tustison NJ
#' @examples
#'
#' \dontrun{
#' fn = 'ADNI_137_S_0158_MR_MPR__GradWarp__N3__Scaled_Br_20070306171702344_S20209_I42985BrainSegmentation.nii.gz'
#' img = antsImageRead( fn ) # see antsSurf on github for data
#' wm   = thresholdImage( img, 3, 4 )
#' # just the surface
#' antsrSurf( x=wm, rotationParams = c(270, 0, 90) )
#' # surface and one overlay
#' wm = thresholdImage(img, 3, 4) %>% iMath("FillHoles")
#' wms = smoothImage( wm, 1.0 )
#' wmk = weingartenImageCurvature( wms, 1.5, 0 )
#' antsrSurf( x=wm, y = list( wmk ), rotationParams = c(270, 0, 90), quantlimits=c(-0.5,0.5) )
#' # surface and two overlays
#' blob = antsImageRead( "manualBlob.nii.gz" ) %>% smoothImage( 1 )
#' antsrSurf( x=wm, y = list( wmk, blob ), colormap=c("red","blue"),
#'   rotationParams = c(270, 0, 90),  quantlimits=c(0.1,0.9))
#'
#' }
#'
#' @export antsrSurf
antsrSurf <- function( x, y,
  quantlimits = c(0.1,0.9),
  colormap = 'jet',
  inflationFactor = 25,
  rotationParams = c(270,0,270),
  verbose = FALSE )
{
domainImageMap = NA
# #' @param domainImageMap resamples surf and func to this domain FIXME
# check for needed programs
ass = system("which antsSurf", intern=TRUE )
cvt = system("which ConvertScalarImageToRGB", intern=TRUE )
if ( length( ass ) == 0 ) stop("This function needs antsSurf in the environment.")
if ( length( cvt ) == 0  ) stop("This function needs ConvertScalarImageToRGB in the environment.")
if ( ! any( is.na( domainImageMap ) ) )
  {
  if ( is.antsImage( domainImageMap ) )
    {
    tx = new("antsrTransform", precision="float",
      type="AffineTransform", dimension = x@dimension )
    x = applyAntsrTransformToImage(tx, x, domainImageMap )
    if ( ! missing( "y" ) )
      {
      if ( is.antsImage( y ) ) y <- list(y)
      for ( i in 1:length( y ) )
        y[[ i ]] =
          applyAntsrTransformToImage(tx, y[[ i ]], domainImageMap,
            interpolation = 'Linear' )
      }
    }
  }
xfn = tempfile( fileext = ".nii.gz" )
antsImageWrite( x, xfn )
# xfn=" wm.nii.gz "       # for testing
# kblobfn=" kblob.nii.gz "  # for testing
asscmd = paste( "antsSurf -s [ ",xfn,",255x255x255] ")
if ( ! missing( y ) )
{
ct = 0
if ( length( colormap ) != length( y ) )
  colormap = rep( colormap, length.out = length( y ) )
for ( overlay in y )
  {
  ct = ct + 1
  wms = smoothImage( overlay, 1.0 )
  myquants = quantile( wms[ abs(wms) > 0 ] , c( 0.001, 1) )
  kblob = thresholdImage( wms, myquants[1], Inf )
  kblobfn = tempfile( fileext = ".nii.gz" )
  antsImageWrite( kblob, kblobfn )
  overlayfn = tempfile(fileext = ".nii.gz" )
  antsImageWrite( overlay, overlayfn )
  csvlutfn = tempfile(fileext = ".csv" )
  overlayrgbfn = tempfile(fileext = ".nii.gz" )
  if ( verbose ) print( colormap[ct] )
  myquants = quantile( overlay[ abs(overlay) > 0 ], quantlimits )
#  overlay[ overlay < myquants[1] ] = myquants[1]
#  overlay[ overlay > myquants[2] ] = myquants[2]
  cvtcmd = paste( "ConvertScalarImageToRGB 3 ",overlayfn, overlayrgbfn,
    kblobfn, colormap[ct]," none ", myquants[1],myquants[2]," 0 255", csvlutfn )
  sss = system( cvtcmd )
  if ( verbose ) print( cvtcmd )
  if ( verbose ) cat( "\n" )
  asscmd = paste( asscmd , "-f [ ",overlayrgbfn,", ",kblobfn,", 0.5 ] ")
  }
}
asscmd = paste( asscmd , " -i ", inflationFactor,
  " -d [",paste( rotationParams, collapse='x' ),",255x255x255] " )
if ( verbose ) print( asscmd )
sss = system( asscmd )
}
