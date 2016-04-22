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
#' @param rotationParams 3 Rotation angles expressed in degrees or a matrix of
#' rotation parameters that will be applied in sequence.
#' @param overlayLimits absolute lower and upper limits for functional overlay.
#' this parameter will override \code{quantlimits}.  Currently, this will set
#' levels above \code{overlayLimits[2]} to \code{overlayLimits[2]}.
#' @param filename prefix filename for output pngs
#' @param verbose prints the command used to call \code{antsSurf}
#' @return no output
#' @author Avants BB, Tustison NJ
#' @examples
#'
#' \dontrun{
#' ch2i = antsImageRead( getANTsRData("mni") )
#' ch2seg = thresholdImage( ch2i, "Otsu", 3 )
#' wm   = thresholdImage( ch2seg, 3, 3 )
#' wm2 = smoothImage( wm, 1 ) %>% thresholdImage( 0.2, Inf )
#' kimg = weingartenImageCurvature( ch2i, 1.5  ) %>% smoothImage( 1 )
#' rp1 = matrix( c(90,180,90), ncol = 3 )
#' rp2 = matrix( c(90,180,270), ncol = 3 )
#' rp3 = matrix( c(90,180,180), ncol = 3 )
#' rp  = rbind( rp1, rp3, rp2 )
#' antsrSurf( wm2, list( kimg ), inflationFactor=55, quantlimits=c(0.01,0.99),rotationParams = rp )
#'
#' # show how to use absolute scales to allow comparison across renderings
#' kimg = thresholdImage( ch2seg , 1, Inf )
#' nvox = sum( ch2seg > 0 )
#' voxvalsmat = cbind(
#'     rnorm( nvox, 2.5, 2 ),
#'     rnorm( nvox, 3.0, 2 ),
#'     rnorm( nvox, 3.5, 2 ) )
#' qq = quantile( voxvalsmat, 0.9 )
#' for ( mmm in 1:3 )
#' {
#' kimg[ ch2seg > 0 ] = as.numeric(voxvalsmat[,mmm])
#' kimg = smoothImage( kimg, 3 )
#' antsrSurf( wm2, list( kimg ), inflationFactor=55, overlayLimits=c( 2.0, qq ),
#'     rotationParams = rp, filename=paste('~/Downloads/ztempX',mmm,sep=''),
#'     verbose=TRUE )
#' }
#'
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
  overlayLimits = NA,
  filename = NA,
  verbose = FALSE )
{
domainImageMap = NA
# #' @param domainImageMap resamples surf and func to this domain FIXME
# check for needed programs
# first get antspath
myantspath = Sys.getenv(c("ANTSPATH"))
if ( length( myantspath ) == 0 ) stop("Please set ANTSPATH in your environment")
ass = file.exists( paste(myantspath,"antsSurf",sep='/') )
cvt = file.exists( paste(myantspath,"ConvertScalarImageToRGB",sep='/') )
if ( length( ass ) == 0 ) stop("This function needs antsSurf in the environment.")
if ( length( cvt ) == 0  ) stop("This function needs ConvertScalarImageToRGB in the environment.")
ass = paste(myantspath,"antsSurf",sep='/')
cvt = paste(myantspath,"ConvertScalarImageToRGB",sep='/')
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
if ( is.na( filename ) ) filename = tempfile( )
if ( ! is.matrix( rotationParams ) )
  rotationParams = matrix( rotationParams, ncol=3 )
if ( nrow( rotationParams ) > 1 )
  {
  if ( !  usePkg("abind") | !  usePkg("png") | !  usePkg("grid"))
    {
    print("Need abind, grid and png")
    return(NULL)
    }
  }
pngs = rep( NA, nrow( rotationParams ) )
for( myrot in 1:nrow( rotationParams ) )
{
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
  myquants = quantile( overlay[ abs(overlay) > 0 ], quantlimits )
  if ( ! all( is.na( overlayLimits ) ) )
    {
    myquants = overlayLimits
    overlay[ overlay < myquants[1] ] = 0
    overlay[ overlay > myquants[2] ] = myquants[2]
    if ( verbose ) print( myquants )
    }
  kblob = thresholdImage( wms, myquants[1], Inf )
  kblobfn = tempfile( fileext = ".nii.gz" )
  antsImageWrite( kblob, kblobfn )
  overlayfn = tempfile(fileext = ".nii.gz" )
  antsImageWrite( overlay, overlayfn )
  csvlutfn = tempfile(fileext = ".csv" )
  overlayrgbfn = tempfile(fileext = ".nii.gz" )
  if ( verbose ) print( colormap[ct] )
  cvtcmd = paste( "ConvertScalarImageToRGB 3 ",overlayfn, overlayrgbfn,
    kblobfn, colormap[ct]," none ", myquants[1],myquants[2]," 0 255", csvlutfn )
  sss = system( cvtcmd )
  if ( verbose ) print( cvtcmd )
  if ( verbose ) cat( "\n" )
  asscmd = paste( asscmd , "-f [ ",overlayrgbfn,", ",kblobfn,", 0.5 ] ")
  }
}
if ( nrow( rotationParams ) == 1 )
{
asscmd = paste( asscmd , " -i ", inflationFactor,
  " -d [",paste( rotationParams[myrot,], collapse='x' ),",255x255x255] " )
} else {
  pngext = myrot
  if ( myrot < 10 ) pngext = paste( "0",pngext,sep='' )
  if ( myrot < 100 ) pngext = paste( "0",pngext,sep='' )
  pngfnloc = paste( filename, pngext, ".png", sep='' )
  system( paste( "rm", pngfnloc ) )
  asscmd = paste( asscmd , " -i ", inflationFactor, " -d ", pngfnloc,
    "[",paste( rotationParams[myrot,], collapse='x' ),",255x255x255] ",sep='' )
}
if ( verbose ) print( asscmd )
sss = system( asscmd )
Sys.sleep( 3 )
if ( nrow( rotationParams ) > 1 ) pngs[ myrot ] = pngfnloc
}
Sys.sleep( 3 )
if ( nrow( rotationParams ) > 1 )
  {
  mypng = png::readPNG( pngs[ 1 ] )
  for ( i in 2:length( pngs ) )
    {
    mypng = abind::abind(  mypng, png::readPNG( pngs[ i ] ) , along = 2 )
    }
  png(paste(filename, ".png", sep = ""), width = dim(mypng)[2], height = dim(mypng)[1])
  grid::grid.raster(mypng)
  dev.off()
  }
}
