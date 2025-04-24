#' Reconstruct image from a list of patches.
#'
#' @param patches List or array of patches defining an image.  Patches are assumed
#' to have the same format as returned by \code{extractImagePatches}.
#' @param domainImage Image or mask to define the geometric information of the
#' reconstructed image.  If this is a mask image, the reconstruction will only
#' use patches in the mask.
#' @param strideLength Defines the sequential patch overlap for
#' \code{maxNumberOfPatches = all}.  Can be a image-dimensional vector or a scalar.
#' @param domainImageIsMask boolean specifying whether the domain image is a
#' mask used to limit the region of reconstruction from the patches.
#'
#' @return an ANTs image.
#' @author Tustison NJ
#' @examples
#'
#' library( ANTsR )
#'
#' image <- antsImageRead( getANTsRData( "r16" ) )
#' patchSet <- extractImagePatches( image, c( 64, 64 ), "all", c( 8, 8 ) )
#' imageReconstructed <-
#'   reconstructImageFromPatches( patchSet, image, c( 8, 8 ) )
#' testthat::expect_equal(as.array(image), as.array(imageReconstructed))
#' rm(image); gc()
#' rm(patchSet); gc()
#' rm(imageReconstructed); gc()
#' @export
reconstructImageFromPatches <- function( patches, domainImage,
  strideLength = 1, domainImageIsMask = FALSE )
{
  imageSize <- dim( domainImage )
  dimensionality <- length( imageSize )

  if( dimensionality != 2 && dimensionality != 3 )
    {
    stop( "Unsupported dimensionality." )
    }

  isListPatches <- is.list( patches )

  patchSize <- c()
  numberOfImageComponents <- 1
  if( isListPatches )
    {
    patchDimension <- dim( patches[[1]] )
    patchSize <- patchDimension[1:dimensionality]
    if( length( patchDimension ) > dimensionality )
      {
      numberOfImageComponents <- patchDimension[dimensionality + 1]
      }
    } else {
    patchDimension <- dim( patches )
    patchSize <- patchDimension[2:( 2 + dimensionality - 1 )]
    if( length( patchDimension ) > dimensionality + 1 )
      {
      numberOfImageComponents <- patchDimension[dimensionality + 2]
      }
    }
  midPatchIndex <- round( patchSize / 2 )

  imageArray <- array( data = 0, dim = c( imageSize, numberOfImageComponents ) )

  strideLengthVector <- strideLength
  if( length( strideLength ) == 1 )
    {
    strideLengthVector <- rep.int( strideLength, dimensionality )
    } else if( length( strideLength ) != dimensionality ) {
    stop( paste0( "strideLength is not a scalar or vector of
      length dimensionality." ) )
    } else if( any( strideLength < 1 ) ) {
    stop( paste0( "strideLength must be a positive integer." ) )
    }

  if( domainImageIsMask )
    {
    maskArray <- as.array( domainImage )
    maskArray[maskArray != 0] <- 1
    }

  count <- 1
  if( dimensionality == 2 )
    {
    if( all( strideLengthVector == 1 ) )
      {
      for( i in seq_len( imageSize[1] - patchSize[1] + 1 ) )
        {
        for( j in seq_len( imageSize[2] - patchSize[2] + 1 ) )
          {
          startIndex <- c( i, j )
          endIndex <- startIndex + patchSize - 1

          doAdd <- TRUE
          if( domainImageIsMask )
            {
            if( maskArray[startIndex[1]:endIndex[1], startIndex[2]:endIndex[2]]
              [midPatchIndex[1], midPatchIndex[2]] == 0 )
              {
              doAdd <- FALSE
              }
            }

          if( doAdd )
            {
            patch <- array( data = 0 )
            if( isListPatches )
              {
              patch <- patches[[count]]
              } else {
              if( numberOfImageComponents == 1 )
                {
                patch <- patches[count,,]
                } else {
                patch <- patches[count,,,]
                }
              }

            if( numberOfImageComponents == 1 )
              {
              patch <- array( data = patch, dim = c( dim( patch ), 1 ) )
              }

            imageArray[startIndex[1]:endIndex[1], startIndex[2]:endIndex[2],] <-
              imageArray[startIndex[1]:endIndex[1], startIndex[2]:endIndex[2],] +
              patch
            count <- count + 1
            }
          }
        }
      if( !domainImageIsMask )
        {
        for( i in seq_len( imageSize[1] ) )
          {
          for( j in seq_len( imageSize[2] ) )
            {
            factor <- min( i, patchSize[1], imageSize[1] - i + 1 ) *
              min( j, patchSize[2], imageSize[2] - j + 1 )

            imageArray[i, j,] <- imageArray[i, j,] / factor
            }
          }
        }
      } else {
      countArray <- array( 0, dim = dim( imageArray )[1:dimensionality] )
      for( i in seq.int( from = 1, to = imageSize[1] - patchSize[1] + 1,
        by = strideLengthVector[1] ) )
        {
        for( j in seq.int( from = 1, to = imageSize[2] - patchSize[2] + 1,
          by = strideLengthVector[2] ) )
          {
          startIndex <- c( i, j )
          endIndex <- startIndex + patchSize - 1

          patch <- array( data = 0 )
          if( isListPatches )
            {
            patch <- patches[[count]]
            } else {
            if( numberOfImageComponents == 1 )
              {
              patch <- patches[count,,]
              } else {
              patch <- patches[count,,,]
              }
            }

          if( numberOfImageComponents == 1 )
            {
            patch <- array( data = patch, dim = c( dim( patch ), 1 ) )
            }

          imageArray[startIndex[1]:endIndex[1], startIndex[2]:endIndex[2],] <-
            imageArray[startIndex[1]:endIndex[1], startIndex[2]:endIndex[2],, drop = FALSE] +
            patch
          countArray[startIndex[1]:endIndex[1], startIndex[2]:endIndex[2]] <-
            countArray[startIndex[1]:endIndex[1], startIndex[2]:endIndex[2]] +
            array( data = 1, dim = patchSize )
          count <- count + 1
          }
        }
      countArray[which( countArray == 0 )] <- 1
      for( i in seq_len( numberOfImageComponents ) )
        {
        imageArray[,, i] <- imageArray[,, i] / countArray
        }
      }
    } else {
    if( all( strideLengthVector == 1 ) )
      {
      for( i in seq_len( imageSize[1] - patchSize[1] + 1 ) )
        {
        for( j in seq_len( imageSize[2] - patchSize[2] + 1 ) )
          {
          for( k in seq_len( imageSize[3] - patchSize[3] + 1 ) )
            {
            startIndex <- c( i, j, k )
            endIndex <- startIndex + patchSize - 1

            doAdd <- TRUE
            if( domainImageIsMask )
              {
              if( maskArray[startIndex[1]:endIndex[1], startIndex[2]:endIndex[2],
                startIndex[3]:endIndex[3]][midPatchIndex[1], midPatchIndex[2], midPatchIndex[3]] == 0 )
                {
                doAdd <- FALSE
                }
              }

            if( doAdd )
              {
              patch <- array( data = 0 )
              if( isListPatches )
                {
                patch <- patches[[count]]
                } else {
                if( numberOfImageComponents == 1 )
                  {
                  patch <- patches[count,,,]
                  } else {
                  patch <- patches[count,,,,]
                  }
                }

              if( numberOfImageComponents == 1 )
                {
                patch <- array( data = patch, dim = c( dim( patch ), 1 ) )
                }

              imageArray[startIndex[1]:endIndex[1],
                startIndex[2]:endIndex[2], startIndex[3]:endIndex[3],] <-
                imageArray[startIndex[1]:endIndex[1],
                  startIndex[2]:endIndex[2], startIndex[3]:endIndex[3],, drop = FALSE] +
                patch
              count <- count + 1
              }
            }
          }
        }

      if( !domainImageIsMask )
        {
        for( i in seq_len( imageSize[1] ) )
          {
          for( j in seq_len( imageSize[2] ) )
            {
            for( k in seq_len( imageSize[3] ) )
              {
              factor <- min( i, patchSize[1], imageSize[1] - i + 1 ) *
                min( j, patchSize[2], imageSize[2] - j + 1 ) *
                min( k, patchSize[3], imageSize[3] - k + 1 )

              imageArray[i, j, k,] <- imageArray[i, j, k,] / factor
              }
            }
          }
        }
      } else {
      countArray <- array( 0, dim = dim( imageArray )[1:dimensionality] )
      for( i in seq.int( from = 1, to = imageSize[1] - patchSize[1] + 1,
        by = strideLengthVector[1] ) )
        {
        for( j in seq.int( from = 1, to = imageSize[2] - patchSize[2] + 1,
          by = strideLengthVector[2] ) )
          {
          for( k in seq.int( from = 1, to = imageSize[3] - patchSize[3] + 1,
            by = strideLengthVector[3] ) )
            {
            startIndex <- c( i, j, k )
            endIndex <- startIndex + patchSize - 1

            patch <- array( data = 0 )
            if( isListPatches )
              {
              patch <- patches[[count]]
              } else {
              if( numberOfImageComponents == 1 )
                {
                patch <- patches[count,,,]
                } else {
                patch <- patches[count,,,,]
                }
              }

            if( numberOfImageComponents == 1 )
              {
              patch <- array( data = patch, dim = c( dim( patch ), 1 ) )
              }

            imageArray[startIndex[1]:endIndex[1], startIndex[2]:endIndex[2],
                startIndex[3]:endIndex[3],] <-
              imageArray[startIndex[1]:endIndex[1], startIndex[2]:endIndex[2],
                startIndex[3]:endIndex[3],, drop = FALSE] + patch
            countArray[startIndex[1]:endIndex[1], startIndex[2]:endIndex[2],
                startIndex[3]:endIndex[3]] <-
              countArray[startIndex[1]:endIndex[1], startIndex[2]:endIndex[2],
                startIndex[3]:endIndex[3]] + array( data = 1, dim = patchSize )
            count <- count + 1
            }
          }
        }
      countArray[which( countArray == 0 )] <- 1
      for( i in seq_len( numberOfImageComponents ) )
        {
        imageArray[,,, i] <- imageArray[,,, i] / countArray
        }
      }
    }

  if( dimensionality == 2 )
    {
    imageArray <- aperm( imageArray, c( 3, 1, 2 ) )
    } else {
    imageArray <- aperm( imageArray, c( 4, 1, 2, 3 ) )
    }

  if( numberOfImageComponents == 1 )
    {
    imageArray <- drop( imageArray )
    reconstructedImage <- as.antsImage( imageArray, reference = domainImage )
    }
  else
    {
    reconstructedImage <- as.antsImage( imageArray,
      pixeltype = domainImage@pixeltype,
      spacing = antsGetSpacing( domainImage ),
      origin = antsGetOrigin( domainImage ),
      direction = antsGetDirection( domainImage ),
      components = numberOfImageComponents )
    }

  return( reconstructedImage )
}
