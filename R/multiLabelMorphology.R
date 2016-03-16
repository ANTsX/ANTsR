#' Morphology on multi label images.
#'
#' Wraps calls to iMath binary morphology. Additionally, dilation and closing operations preserve
#' pre-existing labels. The choices of operation are:
#'
#' Dilation: dilates all labels sequentially, but does not overwrite original labels.
#' This reduces dependence on the intensity ordering of adjoining labels. Ordering dependence
#' can still arise if two or more labels dilate into the same space - in this case, the label
#' with the lowest intensity is retained. With a mask, dilated labels are multiplied by the
#' mask and then added to the original label, thus restricting dilation to the mask region.
#'
#' Erosion: Erodes labels independently, equivalent to calling iMath iteratively.
#'
#' Closing: Close holes in each label sequentially, but does not overwrite original labels.
# 
#' Opening: Opens each label independently, equivalent to calling iMath iteratively.
#'
#' 
#' @param labelsIn Input image should contain only 0 for background and positive integers for
#' labels.
#' @param operation One of \code{'MD'}, \code{'ME'}, \code{'MC'}, \code{'MO'}, passed to iMath.
#' @param radius radius of the morphological operation.
#' @param dilationMask Optional binary mask to constrain dilation only (eg dilate cortical
#' label into WM).
#' @param labelList Optional list of labels, to perform operation upon. Defaults to all unique
#' intensities in \code{labels.in}.
#' 
#' @return processed labels, of type antsImage.
#' @author Cook PA
#' 
#' @examples
#' 
#' img <- antsImageRead( getANTsRData("r16") , 2 )
#' labels = getMask(img,1,150) + getMask(img,151,225) * 2
#' labelsDilated = multiLabelMorphology(labels, 'MD', 2)
#'
#' # should see original label regions preserved in dilated version
#' # label N should have mean N and 0 variance
#' labelStats(labelsDilated, labels)
#' 
multiLabelMorphology <- function(labelsIn, operation, radius, dilationMask=NA, labelList=NA ) {

  # If no label list supplied, get a list of labels in the image
  if (length(labelList) == 1 && is.na(labelList)) {
    labelList <- sort(unique(labelsIn[labelsIn > 0]))
  }

  # All labels, whether to be processed or not
  labelsInBinary <- antsImageClone(labelsIn)
  
  labelsInBinary[labelsInBinary > 1] <- 1
  
  # Erosion / opening is simply a case of looping over the input labels
  if (operation == "ME" || operation == "MO" ) {
    output <- antsImageClone(labelsIn)
    
    for (currentLabel in labelList) {
      output <- iMath(output, operation, radius, currentLabel)
    }
    
    return(output)
  }

  # If we have a dilation mask, quickly check it's not some non-binary label(s)
  if (!is.na(dilationMask)) {
    if (max(dilationMask) != 1) {
      stop("Mask is empty or not binary")
    }
  }
  
  # Dilation and closing are more complex, avoid overwriting input labels
  
  output <- antsImageClone(labelsIn)
 
  for (currentLabel in labelList) {
    # This is binary, no longer has intensity currentLabel
    currentLabelRegion = thresholdImage(labelsIn, currentLabel, currentLabel)
    
    # Other labels, don't dilate into these
    otherLabels <- output - currentLabelRegion

    # Dilated or closed
    cLabBinaryMorphed <- iMath(currentLabelRegion, operation, radius, 1)

    # Mask probably useful only for dilation
    if (operation == 'MD' && !is.na(dilationMask)) {
      # This must be formulated so that it works whether the dilation mask includes currentLabelRegion or not
      cLabBinaryMorphedNoOverlap <- thresholdImage(currentLabelRegion + dilationMask * cLabBinaryMorphed - otherLabels, 1, 2)
    }
    else {
      cLabBinaryMorphedNoOverlap <- thresholdImage(cLabBinaryMorphed - otherLabels, 1, 1)
    }

    output <- output + cLabBinaryMorphedNoOverlap * currentLabel

  }

  return(output)

}
