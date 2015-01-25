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
#' @param pvals the already computed pvalue for each component
#' @return The output point coordinates are in approximate Talairach / MNI (or
#' whatever) template space.
#' @author Avants, BB
#' @keywords Talairach, Template, Coordinates
#' @examples
#' 
#' \dontrun{
#'  tem<-antsImageRead('templates/template_brain.nii.gz',3)
#'  temlab<-antsImageRead('temp.nii.gz',3)
#'  temlab2<-antsImageRead('temp2.nii.gz',3)
#'  # try getANTsRData if you have www access
#'  mymni<-list( antsImageRead(getANTsRData('mni'),3), 
#'  	      antsImageRead(getANTsRData('mnib'),3), 
#'               antsImageRead(getANTsRData('mnia'),3) )
#'  mytem<-list(tem,temlab,temlab2)
#'  mynetworkdescriptor<-getMultivariateTemplateCoordinates(  mytem, mymni , convertToTal = TRUE , pvals=c(0.01,0.05) )
#' # output looks like 
#' #     NetworkID   x   y   z t label Brodmann AAL
#' # 1  N1_omnibus  -7   7  11 0     1        0  71
#' # 2     N1_node  50 -12 -21 0     1       20  90
#' # 3     N1_node -29  22   5 0     2       48  29
#' # 4     N1_node  25 -17  36 0     3        0   0
#' # 5     N1_node -25   4  39 0     4        0   0
#' # 6  N2_omnibus   7   1   3 0     1        0   0
#' # 7     N2_node  42 -14 -26 0     1       20  56
#' # 8     N2_node  20   7  -8 0     2       48  74
#' # 9     N2_node  -2  33 -10 0     3       11  31
#' # 10    N2_node -41 -34  23 0     4       48  17
#' # 11    N2_node  50 -37  24 0     5       41  82
#' # 12    N2_node -24  30  24 0     6       48   0
#' }
#' 
#' @export getMultivariateTemplateCoordinates
getMultivariateTemplateCoordinates <- function(imageSetToBeLabeledIn, templateWithLabels, 
  labelnames = NA, outprefix = NA, convertToTal = FALSE, pvals = NA, threshparam = 1, 
  clustparam = 250, identifier) {
  
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
  talregions <- rep(NA, length(imageSetToBeLabeledIn) - 1)
  mytemplate <- imageSetToBeLabeledIn[[1]]
  for (x in 2:length(imageSetToBeLabeledIn)) {
    img <- imageSetToBeLabeledIn[[x]]
    threshimg <- antsImageClone(img)
    thresh <- 1/length(as.array(threshimg))
    ImageMath(threshimg@dimension, threshimg, "abs", threshimg)
    # threshimg[ threshimg > (.Machine$double.eps*2) ]<-1
    meanval <- mean(threshimg[threshimg > (.Machine$double.eps * 2)])
    sdval <- sd(threshimg[threshimg > (.Machine$double.eps * 2)])
    threshval <- (meanval - sdval * threshparam)
    if (threshval < (.Machine$double.eps * 2)) 
      threshval <- (.Machine$double.eps * 2)
    threshimg[threshimg > threshval] <- 1
    threshimg[threshimg <= threshval] <- 0
    imageSetToBeLabeled <- lappend(list(mytemplate), threshimg)
    temp <- getTemplateCoordinates(imageSetToBeLabeled, templateWithLabels, labelnames, 
      outprefix, convertToTal)
    talRegion <- ""
    if (temp$templatepoints$x < 0) 
      talRegion <- paste(talRegion, "L", sep = "") else talRegion <- paste(talRegion, "R", sep = "")
    if (temp$templatepoints$y > 0) 
      talRegion <- paste(talRegion, "A", sep = "") else talRegion <- paste(talRegion, "P", sep = "")
    if (temp$templatepoints$z < 0) 
      talRegion <- paste(talRegion, "S", sep = "") else talRegion <- paste(talRegion, "I", sep = "")
    talregions[x - 1] <- talRegion
    clust <- labelClusters(threshimg, clustparam)
    imageSetToBeLabeled <- lappend(list(mytemplate), clust)
    temp2 <- getTemplateCoordinates(imageSetToBeLabeled, templateWithLabels, 
      labelnames, outprefix, convertToTal)
    if (x == 2) {
      myout <- data.frame(NetworkID = paste("N", identifier[1], "_omnibus", 
        sep = ""), temp$templatepoints, pval = pvals[x - 1])
      subnet <- data.frame(NetworkID = rep(paste("N", identifier[1], "_node", 
        sep = ""), nrow(temp2$templatepoints)), temp2$templatepoints, pval = rep(NA, 
        nrow(temp2$templatepoints)))
      myout <- rbind(myout, subnet)
    } else {
      pre <- paste("N", identifier[x - 1], sep = "")
      mynextout <- data.frame(NetworkID = paste(pre, "_omnibus", sep = ""), 
        temp$templatepoints, pval = pvals[x - 1])
      subnet <- data.frame(NetworkID = rep(paste(pre, "_node", sep = ""), nrow(temp2$templatepoints)), 
        temp2$templatepoints, pval = rep(NA, nrow(temp2$templatepoints)))
      myout <- rbind(myout, mynextout)
      myout <- rbind(myout, subnet)
    }
  }
  return(list(networks = myout, myLabelsInTemplateSpace = temp$myLabelsInTemplateSpace, 
    myImageInTemplateSpace = temp$myImageInTemplateSpace))
} 
