#' Compose arbitrary transform list into a deformation field.
#'
#' A deformation field can capture any domain to domain correspondence that we
#' can represent in ANTsR.  This function will map a list of transforms into a
#' single deformation field object.
#'
#' @param image input image defines the transformation domain
#' @param transforms list of transform filenames or antrTransforms
#' @return field deformation object is output
#' @author Avants BB, Duda JT
#' @examples
#'
#' fi <- antsImageRead(getANTsRData("r16") )
#' mi <- antsImageRead(getANTsRData("r64") )
#' fi<-resampleImage(fi,c(60,60),1,0)
#' mi<-resampleImage(mi,c(60,60),1,0) # speed up
#' mytx <- antsRegistration(fixed=fi, moving=mi, typeofTransform = c('SyN') )
#' compfield = composeTransformsToField( fi, mytx$fwd )
#' atx = antsrTransformFromDisplacementField( compfield )
#' wrped = applyAntsrTransformToImage( atx, mi, fi )
#'
#' @export composeTransformsToField
composeTransformsToField <- function(
  image,
  transforms )
{
  mydim = image@dimension
  # first thing is to convert the transform files to antsr types if they are
  # not already that way
  if ( class( transforms[[1]] ) == "character" )
    {
    txlist = list( )
    for ( k in 1:length( transforms ) )
      {
      isfield = length( grep( ".mat", transforms[ k ] ) ) == 0
      if ( isfield ) {
        field  = antsImageRead( transforms[ k ] )
        txlist[[ k ]] = antsrTransformFromDisplacementField( field )
        } else txlist[[ k ]] = readAntsrTransform( transforms[ k ], dimension=mydim )
      }
    } else txlist = transforms
  comptx = composeAntsrTransforms(  txlist )
  vecimglist = list( )
  itlist = list( )
  for ( k in 1:mydim ) {
    vecimglist[[ k ]] = image * 0
    itlist[[ k ]] = antsImageIterator( vecimglist[[ k ]] )
    }
  while ( !antsImageIteratorIsAtEnd( itlist[[ 1 ]] ) )
    {
    idx = as.numeric( antsImageIteratorGetIndex( itlist[[ 1 ]] ) )
    point = antsTransformIndexToPhysicalPoint( image, idx )
    vec = applyAntsrTransform( comptx, point, reference = image ) - point
#    if ( idx[1]==30 & idx[2]==40 ) {
#      wpts <- antsApplyTransformsToPoints( dim=mydim, points=data.frame(point),
#          transformlist=transforms )
#      vec2 = as.numeric( wpts ) - point
#      print( "idx" )
#      print( vec )
#      print( vec2 )
#      }
    for ( k in 1:mydim )  {
      antsImageIteratorSet( itlist[[ k ]], vec[ k ] )
      itlist[[ k ]] = antsImageIteratorNext( itlist[[ k ]] )
      }
    }
mergeChannels( vecimglist )
}
