#' DesikanKillianyTourville
#'
#' A data frame label numbers and names for the neuroanatomical
#' DesikanKillianyTourville labels.
#'
#' @docType data
#' @references  \url{http://www.ncbi.nlm.nih.gov/pubmed/23227001}
#' @format A data frame listing the following variables.
#' \describe{
#'    \item{\code{label_num}}{ Numerical label value. }
#'    \item{\code{label_name}}{ Shorthand anatomical value. }
#'    \item{\code{tissue}}{ Shorthand tissue value. }
#'    \item{\code{contralateral}}{ contralateral anatomical value. }
#'    \item{\code{lobe}}{ which lobe: temporal, parietal, occipital, frontal.}
#'    \item{\code{hemisphere}}{ left or right hemisphere. }
#' }
#' @examples
#' data(DesikanKillianyTourville)
"DesikanKillianyTourville"

#' BOLD Correlation Matrix
#'
#' A data frame with an example square BOLD correlation matrix.
#'
#' @docType data
#'
#' @format A data frame square correlation matrix listed by AAL
#' anatomical column names.
#' @references \url{http://en.wikipedia.org/wiki/Automated_Anatomical_Labeling}
#' @examples
#' data(bold_correlation_matrix)
"bold_correlation_matrix"

#' @title powers_areal_mni_itk
#' @docType data
#' @description A data frame providing coordinates for the nodes
#' identified by Powers at al. Coordinates have been modified from nifti
#' space to ITK space
#' @format A data frame listing the following variables.
#' \describe{
#'   \item{\code{x}}{ x coordinate }
#'   \item{\code{y}}{ y coordinate }
#'   \item{\code{z}}{ z coordinate }
#'   \item{\code{ROI}}{ unique integer for each node }
#'   \item{\code{SystemLabel}}{ unique integer for each system }
#'   \item{\code{SystemName}}{ unique name for each system }
#'   \item{\code{Color}}{ name of color for system }
#'   \item{\code{r}}{ red value for system }
#'   \item{\code{g}}{ green value for system }
#'   \item{\code{b}}{ blue value for system }
#'   \item{\code{Anatomy}}{ anatomical location as
#'   determined by OASIS labeling }
#'   \item{\code{Lobe}}{ lobe location of node }
#'   \item{\code{Brodmann}}{ Broadmann area number }
#'   \item{\code{AAL}}{ AAL region label }
#' }
#' #' @references \url{http://www.nil.wustl.edu/labs/petersen/Resources.html}
#'
#' @examples
#' data(powers_areal_mni_itk)
#' \dontrun{
#' fixed <- antsImageRead(getANTsRData("ch2"))
#' moving <- antsImageRead(getANTsRData("mni"))
#' mytx <- antsRegistration(
#'   fixed = fixed, moving = moving,
#'   typeofTransform = c("SyN")
#' )
#' data("powers_areal_mni_itk", package = "ANTsR", envir = environment())
#' coords <- powers_areal_mni_itk[, 1:3]
#' ch2reg <- antsRegistration(fixed, moving, typeofTransform = "SyN")
#' coordsw <- antsApplyTransformsToPoints(
#'   dim = 3, points = coords,
#'   transformlist = ch2reg$fwdtransforms,
#'   whichtoinvert = c(FALSE, FALSE)
#' )
#' ptrd <- 3
#' powersLabels <- makePointsImage(coordsw, moving, radius = pard)
#' plot(moving, powersLabels, axis = 3, nslices = 30)
#' }
#' @keywords datasets
"powers_areal_mni_itk"


#' @title tracts
#' @docType data
#' @description A data frame label numbers and names for
#' the white matter tracts labels.
#' @format A data frame listing the following variables.
#' \describe{
#' \item{\code{label_num}}{ Numerical label value. }
#' \item{\code{label_name}}{ Shorthand anatomical value. }
#' }
#' @references \url{http://www.ncbi.nlm.nih.gov/pubmed/18407524}
#' @examples data(tracts)
#' @keywords datasets
"tracts"


#' @title AAL Labels
#' @docType data
#' @description A data frame label numbers and names
#' for the neuroanatomical AAL labels.
#' @format A data frame listing the following variables.
#' \describe{
#'   \item{\code{label_num}}{ Numerical label value. }
#'   \item{\code{label_name}}{ Shorthand anatomical value. }
#'   \item{\code{cerebellum}}{binary value}
#'   \item{\code{cortex}}{binary value}
#'   \item{\code{deepgrey}}{binary value}
#'   \item{\code{frontal}}{binary value}
#'   \item{\code{isdmn}}{binary value}
#'   \item{\code{issalience}}{binary value}
#'   \item{\code{left}}{binary value}
#'   \item{\code{limbic}}{binary value}
#'   \item{\code{occipital}}{binary value}
#'   \item{\code{parietal}}{binary value}
#'   \item{\code{right}}{binary value}
#'   \item{\code{temporal}}{binary value}
#'   \item{\code{unknown}}{binary value}
#' }
#' @references
#' \url{http://en.wikipedia.org/wiki/Automated_Anatomical_Labeling}
#' @examples
#' data(aal)
#' @keywords datasets
"aal"


#' @name antsrVersions
#' @title antsrVersions
#' @docType data
#' @description A data frame defining the git tag
#' for the current ANTsR version.  One can also see
#' antsrVersions from ANTsRCore to get the git tag for
#' those dependencies.
#' @format   A data frame listing the following variables.
#' \describe{
#'   \item{\code{Dependency}}{ Name of software dependency. }
#'   \item{\code{GitTag}}{ The git tag.  This can also be used to
#'   trace other dependencies, e.g. the ITK version used by the
#'   current ANTs version. }
#' }
#' @references \url{https://github.com/stnava/ANTs}
#' @examples data(antsrVersions)
#' @keywords datasets
NULL
