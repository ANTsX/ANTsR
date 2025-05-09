#' One-hot encoding function
#'
#' Basic one-hot transformation of segmentations array.
#'
#' @param segmentationsArray multi-label numpy array
#' @param segmentationLabels vector of segmentation labels.  Note that a
#' background label (typically 0) needs to be included.
#' @param channelFirstOrdering Specifies the ordering of the dimensions.
#'
#' @return an n-D array of shape where the segmentation labels are expanded in 
#' a one-hot fashion in the channels dimension.
#'
#' @examples
#'
#' r16 <- antsImageRead( getANTsRData( "r16" ) )
#' seg <- kmeansSegmentation( r16, 3 )$segmentation
#' oneHot <- segmentationToOneHot( as.array( seg ) )
#'
#' r16 <- antsImageRead( getANTsRData( "r16" ) )
#' seg <- kmeansSegmentation( r16, 3 )$segmentation
#' oneHot <- segmentationToOneHot( as.array( seg ),
#'                                 channelFirstOrdering = TRUE )
#'
#' @author Tustison NJ
#' @export
segmentationToOneHot <- function( segmentationsArray, 
                                  segmentationLabels = NULL,
                                  channelFirstOrdering = FALSE )
{
  if( is.null( segmentationLabels ) )
    {
    segmentationLabels <- unique( as.vector( segmentationsArray ) )
    segmentationLabels <- segmentationLabels[order( segmentationLabels )]
    }
  numberOfLabels <- length( segmentationLabels )
  if( numberOfLabels < 2 )
    {
    stop( "At least two segmentation labels need to be specified." )
    }
  imageDimension <- length( dim( segmentationsArray ) )

  oneHotArray <- array( 0, dim = c( dim( segmentationsArray ), numberOfLabels ) )
  for( i in seq.int( numberOfLabels ) )
    {
    perLabel <- array( 0, dim = dim( segmentationsArray ))
    perLabel[which( segmentationsArray == segmentationLabels[i] )] <- 1L
    if( imageDimension == 2 )
      {
      oneHotArray[,,i] <- perLabel
      } else if( imageDimension == 3 ) {
      oneHotArray[,,,i] <- perLabel
      } else {
      stop( "Unrecognized image dimensionality." )
      }
    }

  if( channelFirstOrdering )
    {
    if( imageDimension == 2 )
      {
      oneHotArray <- aperm( oneHotArray, c( 3, 1, 2 ) )
      } else if( imageDimension == 3 ) {
      oneHotArray <- aperm( oneHotArray, c( 4, 1, 2, 3 ) )
      }
    }

  return( oneHotArray )
}

#' Inverse one-hot transformation
#'
#' Inverse of basic one-hot transformation of segmentations array
#'
#' @param oneHotArray an array where the channel dimension contains the 
#' one-hot encoding.
#' @param domainImage image definining the geometry of the returned probability
#' images.
#' @param channelFirstOrdering Specifies the ordering of the dimensions.
#'
#' @return a list of probability images.
#' @examples
#'
#' r16 <- antsImageRead( getANTsRData( "r16" ) )
#' seg <- kmeansSegmentation( r16, 3 )$segmentation
#' oneHot <- segmentationToOneHot( as.array( seg ) )
#' oneHotInv <- oneHotToSegmentation( oneHot, seg )
#'
#' r16 <- antsImageRead( getANTsRData( "r16" ) )
#' seg <- kmeansSegmentation( r16, 3 )$segmentation
#' oneHot <- segmentationToOneHot( as.array( seg ),
#'                                 channelFirstOrdering = TRUE )
#' oneHotInv <- oneHotToSegmentation( oneHot, seg,
#'                                    channelFirstOrdering = TRUE ) 
#'
#' @author Tustison NJ
#' @export
oneHotToSegmentation <- function( oneHotArray, 
                                  domainImage,
                                  channelFirstOrdering = FALSE )
{
  numberOfLabels <- tail( dim( oneHotArray ), 1 )
  if( channelFirstOrdering )
    {
    numberOfLabels <- dim( oneHotArray )[1]
    }

  imageDimension <- domainImage@dimension

  probabilityImages <- list()
  for( label in seq_len( numberOfLabels ) )
    {
    if( imageDimension == 2 )
      {
      if( channelFirstOrdering )
        {
        imageArray <- oneHotArray[label,,]
        } else {
        imageArray <- oneHotArray[,,label]
        }
      } else {
      if( channelFirstOrdering )
        {
        imageArray <- oneHotArray[label,,,]
        } else {
        imageArray <- oneHotArray[,,,label]
        }
      }
    probabilityImages[[label]] <- as.antsImage( imageArray,
      reference = domainImage )
    }
  return( probabilityImages )
}




