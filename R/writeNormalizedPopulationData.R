#' Save normalized population data to a h5 file
#'
#' This function simplifies the steps of saving a population (image) analysis
#' into an efficient hdf5 file, using the h5 library.  The population image data
#' is stored as a matrix.  Along with the matrix will be a data frame that
#' contains the population demographics.  The number of matrix columns will be
#' defined by a mask that the user should also supply.  Finally, a boolean
#' vector should be passed in that matches the images to the demographics. This
#' function cannot check if the matching between demographics and images is
#' correct and, as such, the user should take care in creating the inputs.
#' WARNING: the demographics file will currently be cast to a data.matrix so
#' one should not reuse this file as the reference demographics file.
#'
#' @param demographics data frame that identifies the variables of interest.
#' @param imageMat a matrix that stores the normalized image data.
#' @param imageMask mask with number of non-zero entries defining the matrix columns.
#' @param imageBoolean a vector of booleans with length equal to the number of
#' rows in the demographics data frame and number of true values equal to the
#' number of rows in the image matrix.
#' @param filename output filename for hdf5 file.
#' @return successOrFailure boolean
#' @author Avants BB
#' @examples
#'
#'
#' ilist = getANTsRData( "population" )
#' mask = getMask( ilist[[ 1 ]] )
#' imat = imageListToMatrix( ilist, mask )
#' demog = data.frame( id = c("A","B","C",NA),
#'   age = c( 11, 7, 18, 22 ), sex = c("M","M","F","F") )
#' ibool = c( TRUE, TRUE, TRUE, FALSE )
#' if ( usePkg( "h5" ) ) writeNormalizedPopulationData( demog, imat, mask, ibool,
#'   tempfile(fileext=".h5") )
#'
#' @export writeNormalizedPopulationData
writeNormalizedPopulationData <- function(
  demographics,
  imageMat,
  imageMask,
  imageBoolean,
  filename )
{
if ( ! usePkg( "h5" ) )
  stop( "Please install package h5 in order to use this function." )
if ( sum( imageMask > 0.5 ) != ncol( imageMat ) )
  stop( "stopping because sum( imageMask > 0.5 ) != ncol( imageMat )" )
if ( sum( imageBoolean ) != nrow( imageMat ) )
  stop( "stopping because sum( imageBoolean ) != nrow( imageMat )" )
if ( length( imageBoolean ) != nrow( demographics ) )
  stop( "stopping because length( imageBoolean ) != nrow( demographics )" )
if ( file.exists( filename ) )
  stop( "stopping because file.exists( filename )" )
#
# save.ANTsR(
#  filename = filename,
#  objects = c( "demographics", "imageMat", "imageMask", "imageBoolean" ),
#  overwrite = F, clonediskfiles=T
#  )
# return( TRUE )
#
file <- h5file( filename )
file["antsrpopdata/demographics"] <- data.matrix( demographics )
h5attr(file["antsrpopdata/demographics"], "colnames") <- colnames(demographics)
file["antsrpopdata/imageMat"] <- imageMat
file["antsrpopdata/imageMask"] <- as.array( imageMask )
h5attr(file["antsrpopdata/imageMask"], "spacing") <- antsGetSpacing( imageMask )
h5attr(file["antsrpopdata/imageMask"], "direction") <- antsGetDirection( imageMask )
h5attr(file["antsrpopdata/imageMask"], "origin") <- antsGetOrigin( imageMask )
file["antsrpopdata/imageBoolean"] <- imageBoolean
file["antsrpopdata/filename"] <- filename
h5close(file)
return( TRUE )
}




#' Read normalized population from h5 file
#'
#' This function reads a file created by \code{writeNormalizedPopulationData}.
#'
#' @param filename input filename for hdf5 file.
#' @return list containing demographics, imageMat, imageMask and imageBoolean
#' @author Avants BB
#' @examples
#'
#' ilist = getANTsRData( "population" )
#' mask = getMask( ilist[[ 1 ]] )
#' imat = imageListToMatrix( ilist, mask )
#' demog = data.frame( age = c( 11, 7, 18, 22 ), sex = c("M","M","F","F") )
#' ibool = c( TRUE, TRUE, TRUE, FALSE )
#' tfn = tempfile(fileext=".h5")
#' if ( usePkg( "h5" ) ) writeNormalizedPopulationData( demog, imat, mask, ibool, tfn )
#' if ( usePkg( "h5" ) ) dlist = readNormalizedPopulationData( tfn )
#'
#' @export readNormalizedPopulationData
readNormalizedPopulationData <- function( filename )
{
  if ( ! usePkg( "h5" ) )
    stop( "Please install package h5 in order to use this function." )
  file <- h5file( filename )
  demog <- file["antsrpopdata/demographics"]
  demographics <- demog[]
  h5attr( demog, "colnames" )
  colnames( demographics ) <- h5attr( demog, "colnames" )
  temp <- file["antsrpopdata/imageMat"]
  imageMat <- temp[]
  temp <- file["antsrpopdata/imageMask"]
  imageMask <- as.antsImage( temp[] )
  k=antsSetSpacing( imageMask, h5attr( temp, "spacing" ) )
  k=antsSetOrigin( imageMask, h5attr( temp, "origin" ) )
  k=antsSetDirection( imageMask, h5attr( temp, "direction" ) )
  temp <- file["antsrpopdata/imageBoolean"]
  imageBoolean <- temp[]
  h5close(file)
  return(
      list(
        demographics   = data.frame( demographics ),
        imageMat       = imageMat,
        imageMask      = imageMask,
        imageBoolean   = imageBoolean
        )
    )
}
