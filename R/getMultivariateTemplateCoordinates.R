#' Label multivariate components by an anatomical coordinate system.
#'
#' This function will provide a mapping that labels the list of input images
#' and each of their blobs.
#'
#' Uses getTemplateCoordinates as a sub-routine.
#'
#' TBN
#'
#' @param imageSetToBeLabeledIn a template paired with (most likely) the output
#' of a multivariate sparse decomposition or (alternatively) could be just a
#' statistical map with zeroes in non-interesting areas
#' @param templateWithLabels e..g the mni image and brodmann label set
#' @param labelnames a list of names for the labels
#' @param outprefix if output to a file, provide file prefix
#' @param convertToTal bool, return talairach coordinates
#' @param pvals the already computed pvalue for each component
#' @param threshparam for pvals
#' @param clustparam for clusters
#' @param identifier unique ID for this study
#' @return The output point coordinates are in approximate Talairach / MNI (or
#' whatever) template space.
#' @author Avants, BB
#' @keywords Talairach, Template, Coordinates
#' @examples
#'
#' \dontrun{
#'  tem<-antsImageRead( getANTsRData("ch2") )
#'  temlab<-antsImageRead( getANTsRData("ch2b")  )
#'  temlab2<-antsImageRead( getANTsRData("ch2a")  )
#'  # try getANTsRData if you have www access
#'  mymni<-list( antsImageRead(getANTsRData("mni"),3),
#'        antsImageRead(getANTsRData("mnib"),3),
#'        antsImageRead(getANTsRData("mnia"),3) )
#'  mytem<-list( smoothImage(tem,3) ,temlab,temlab2)
#'  # mynetworkdescriptor<-getMultivariateTemplateCoordinates(
#'  #  mytem, mymni , convertToTal = TRUE , pvals=c(0.01,0.05) )
#' }
#'
#' @export getMultivariateTemplateCoordinates
getMultivariateTemplateCoordinates <- function(
  imageSetToBeLabeledIn,
  templateWithLabels,
  labelnames = NA,
  outprefix = NA,
  convertToTal = FALSE,
  pvals = NA,
  threshparam = 1,
  clustparam = 250,
  identifier ) {

  # this function is similar to getTemplateCoordinates however we need to get the
  # coordinates for each of the entries in imageSetToBeLabeled where coordinates
  # come from each subcomponent of the image in imageSetToBeLabeled pseudo-code: 1.
  # transform template to talairach 2. label the multivariate component image by
  # its generic Talairach position i.e. L/R, S/I, A/P --- information needed for
  # this is given by getTemplateCoordinates 3. split each image into its sub
  # components via image2ClusterImages 4. get coordinates for each sub-component
  # image 5. append these coordinates under the output from step 2 6. return
  # something e.g. a table ....

  if (missing(identifier)) {
    identifier <- c(1:(length(imageSetToBeLabeledIn) - 1))
  }
  if (is.na(pvals[1])) {
    pvals <- rep(NA, length(imageSetToBeLabeledIn) - 1)
  }
  myout <- NA
  mytemplate <- imageSetToBeLabeledIn[[1]]
  for (x in 2:length(imageSetToBeLabeledIn)) {
    img <- imageSetToBeLabeledIn[[x]]
    threshimg <- abs( antsImageClone(img) )
    clust <- labelClusters(threshimg, minClusterSize=clustparam,
      minThresh = 1e-6, maxThresh = Inf )
    imageSetToBeLabeled <- lappend(list(mytemplate), clust)
    temp2 <- getTemplateCoordinates(
      imageSetToBeLabeled, templateWithLabels,labelnames,
      outprefix, convertToTal)
    pre <- paste("N", identifier[x - 1], sep = "")
    subnet <- data.frame(NetworkID = rep(paste(pre, "_node", sep = ""),
      nrow(temp2$templatepoints)),
      temp2$templatepoints, pval = c( round(pvals[x - 1]*1.e6)/1.e6,
        rep("",nrow(temp2$templatepoints)-1) ) )
    if ( all( is.na( myout ) ) ) myout = subnet
      else myout <- rbind(myout, subnet)
  }
  return(list(networks = myout, myLabelsInTemplateSpace = temp2$myLabelsInTemplateSpace,
    myImageInTemplateSpace = temp2$myImageInTemplateSpace))
}
