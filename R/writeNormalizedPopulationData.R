#' Save normalized population data to a directory or h5 file
#'
#' This function simplifies the steps of saving a population (image) analysis
#' into an efficient hdf5 file, using the h5 library or to a directory.  The
# 'population image data is stored as a matrix.  Along with the matrix will be a
#' data frame with population demographics. The number of matrix columns will be
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
#' @param filename output filename for hdf5 file (if .h5) or directory name.
#' @return successOrFailure boolean
#' @author Avants BB
#' @examples
#' # read below will test out writing as well
#' \dontrun{
#' ilist = getANTsRData( "population" )
#' mask = getMask( ilist[[ 1 ]] )
#' imat = imageListToMatrix( ilist, mask )
#' demog = data.frame( id = c("A","B","C",NA),
#'   age = c( 11, 7, 18, 22 ), sex = c("M","M","F","F") )
#' ibool = c( TRUE, TRUE, TRUE, FALSE )
#' tfn = tempfile(fileext=".h5")
#' if ( usePkg( "hdf5r" ) ) {
#' demographics = demog
#' imageMat = imat 
#' imageMask = mask
#' imageBoolean = ibool
#' filename = tfn
#' writeNormalizedPopulationData( demog, imat, mask, ibool,
#'   tfn )
#'   }
#' }
#' @export writeNormalizedPopulationData
writeNormalizedPopulationData <- function(
  demographics,
  imageMat,
  imageMask,
  imageBoolean,
  filename )
{
  outputToDir = length( grep( "h5", filename ) ) == 0
  if ( !outputToDir ) if ( ! usePkg( "hdf5r" ) )
    stop( "Please install package hdf5r in order to use this function." )
  if ( sum( imageMask > 0.5 ) != ncol( imageMat ) )
    stop( "stopping because sum( imageMask > 0.5 ) != ncol( imageMat )" )
  if ( sum( imageBoolean ) != nrow( imageMat ) )
    stop( "stopping because sum( imageBoolean ) != nrow( imageMat )" )
  if ( length( imageBoolean ) != nrow( demographics ) )
    stop( "stopping because length( imageBoolean ) != nrow( demographics )" )
  if ( file.exists( filename ) )
    stop( "stopping because file.exists( filename )" )
  if ( outputToDir )  {
    dir.create( filename, showWarnings = TRUE, recursive = FALSE, mode = "0777")
    write.csv( demographics, paste( filename, '/demog.csv', sep=''), row.names = FALSE )
    haveImageDf = data.frame( haveImage = imageBoolean )
    write.csv( haveImageDf, paste( filename, '/haveImage.csv', sep=''), row.names = FALSE )
    antsImageWrite( imageMask, paste( filename, '/imageMask.nii.gz', sep='') )
    antsImageWrite( as.antsImage( imageMat ), paste( filename, '/imageMat.mha', sep='') )
    return( TRUE )
  }
  file <- hdf5r::h5file( filename )
  file$create_group("antsrpopdata")
  file[["antsrpopdata/demographics"]] <- data.matrix( demographics )
  hdf5r::h5attr(file[["antsrpopdata/demographics"]], "colnames") <- colnames(demographics)
  file[["antsrpopdata/imageMat"]] <- imageMat
  file[["antsrpopdata/imageMask"]] <- as.array( imageMask )
  hdf5r::h5attr(file[["antsrpopdata/imageMask"]], "spacing") <- antsGetSpacing( imageMask )
  hdf5r::h5attr(file[["antsrpopdata/imageMask"]], "direction") <- antsGetDirection( imageMask )
  hdf5r::h5attr(file[["antsrpopdata/imageMask"]], "origin") <- antsGetOrigin( imageMask )
  file[["antsrpopdata/imageBoolean"]] <- imageBoolean
  file[["antsrpopdata/filename"]] <- filename
  hdf5r::h5close(file)
  return( TRUE )
}




#' Read normalized population from h5 file or directory
#'
#' This function reads a file created by \code{writeNormalizedPopulationData}.
#'
#' @param filename input filename for hdf5 file.
#' @return list containing demographics, imageMat, imageMask and imageBoolean
#' @author Avants BB
#' @examples
#' tfn = system.file("extdata", "normpop.h5", package = "ANTsR")
#' if (file.exists(tfn)) {
#'     dlist = readNormalizedPopulationData( tfn)
#' }
#' \dontrun{
#' ilist = getANTsRData( "population" )
#' mask = getMask( ilist[[ 1 ]] )
#' imat = imageListToMatrix( ilist, mask )
#' demog = data.frame( age = c( 11, 7, 18, 22 ), sex = c("M","M","F","F") )
#' ibool = c( TRUE, TRUE, TRUE, FALSE )
#' tfn = tempfile(fileext=".h5")
#' if ( usePkg( "hdf5r" ) ) writeNormalizedPopulationData( demog, imat, mask, ibool, tfn )
#' if ( usePkg( "hdf5r" ) ) dlist = readNormalizedPopulationData( tfn )
#' }
#' @export readNormalizedPopulationData
readNormalizedPopulationData <- function( filename )
{
  outputToDir = length( grep( "h5", filename ) ) == 0
  if ( !outputToDir ) if ( ! usePkg( "hdf5r" ) )
    stop( "Please install package hdf5r in order to use this function." )
  if ( outputToDir ) {
    if ( ! dir.exists( filename ) )
      stop( paste( filename, "directory does not exist." ) )
    demographics = read.csv( paste( filename, '/demog.csv', sep='')  )
    imageBoolean = read.csv( paste( filename,  '/haveImage.csv', sep='') )$haveImage
    imageMask = antsImageRead( paste( filename, '/imageMask.nii.gz', sep='') )
    imageMat = as.matrix( antsImageRead( paste( filename, '/imageMat.mha', sep='') ) )
  } else {
    file <- hdf5r::h5file( filename )
    demog <- file[["antsrpopdata/demographics"]]
    demographics <- demog$read()
    hdf5r::h5attr( demog, "colnames" )
    colnames( demographics ) <- hdf5r::h5attr( demog, "colnames" )
    temp <- file[["antsrpopdata/imageMat"]]
    imageMat <- temp$read()
    temp <- file[["antsrpopdata/imageMask"]]
    imageMask <- as.antsImage( temp[,] )
    k=antsSetSpacing( imageMask, hdf5r::h5attr( temp, "spacing" ) )
    k=antsSetOrigin( imageMask, hdf5r::h5attr( temp, "origin" ) )
    k=antsSetDirection( imageMask, hdf5r::h5attr( temp, "direction" ) )
    temp <- file[["antsrpopdata/imageBoolean"]]
    imageBoolean <- temp$read()
    hdf5r::h5close(file)
  }
  return(
    list(
      demographics   = data.frame( demographics ),
      imageMat       = imageMat,
      imageMask      = imageMask,
      imageBoolean   = imageBoolean
    )
  )
}
