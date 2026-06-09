#' DesikanKillianyTourville
#'
#' A data frame label numbers and names for the neuroanatomical
#' DesikanKillianyTourville labels.
#'
#' @docType data
#' @references  \url{https://pubmed.ncbi.nlm.nih.gov/23227001}
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
#' @references \url{https://en.wikipedia.org/wiki/Automated_Anatomical_Labeling}
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
#' @references Power, J. D., et al. (2011). "Functional network organization of the human brain." Neuron 72(4): 665-678.
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
#' @references \url{https://pubmed.ncbi.nlm.nih.gov/18407524}
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
#' \url{https://en.wikipedia.org/wiki/Automated_Anatomical_Labeling}
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
#' @references \url{https://github.com/ANTsX/ANTs}
#' @examples data(antsrVersions)
#' @keywords datasets
NULL

#' @title cit168_reinf_learn
#' @docType data
#' @description A data frame label numbers and names for the CIT168 reinforcement learning atlas.
#' @format A data frame with columns: Label, Description.
"cit168_reinf_learn"

#' @title cit168_brainstem
#' @docType data
#' @description A data frame label numbers and names for the CIT168 brainstem atlas.
#' @format A data frame with columns: Label, Description, Side.
"cit168_brainstem"

#' @title jhu_fa_labels
#' @docType data
#' @description A data frame label numbers and names for the JHU FA white matter labels.
#' @format A data frame with columns: Label, Description.
"jhu_fa_labels"

#' @title jhu_md_labels
#' @docType data
#' @description A data frame label numbers and names for the JHU MD white matter labels.
#' @format A data frame with columns: Label, Description.
"jhu_md_labels"

#' @title lobes_brainstem
#' @docType data
#' @description A data frame label numbers and names for lobes and brainstem regions.
#' @format A data frame with columns: Label, Description, Side.
"lobes_brainstem"

#' @title basal_forebrain
#' @docType data
#' @description A data frame label numbers and names for basal forebrain regions.
#' @format A data frame with columns: Label, Description, Side.
"basal_forebrain"

#' @title cerebellum_labels
#' @docType data
#' @description A data frame label numbers and names for cerebellum regions.
#' @format A data frame with columns: Label, Description.
"cerebellum_labels"

#' @title dkt_labels
#' @docType data
#' @description A data frame label numbers and names for DKT cortical labels.
#' @format A data frame with columns: Label, Description, type.
"dkt_labels"

#' @title dkt_cit_labels
#' @docType data
#' @description A data frame label numbers and names for consolidated DKT and CIT labels.
#' @format A data frame with columns: Label, Description.
"dkt_cit_labels"

#' @title lobes
#' @docType data
#' @description A data frame label numbers and names for brain lobes.
#' @format A data frame with columns: Label, Description, Side, SideNumber.
"lobes"

#' @title mtl_labels
#' @docType data
#' @description A data frame label numbers and names for medial temporal lobe regions.
#' @format A data frame with columns: Label, Description.
"mtl_labels"

#' @title nbm3_labels
#' @docType data
#' @description A data frame label numbers and names for NBM3 regions.
#' @format A data frame with columns: Label, Description, Side.
"nbm3_labels"

#' @title tissues
#' @docType data
#' @description A data frame label numbers and names for tissue types.
#' @format A data frame with columns: Label, Description, ProbabilityNumber.
"tissues"

#' @title wm_major_tracts
#' @docType data
#' @description A data frame label numbers and names for major white matter tracts.
#' @format A data frame with columns: Label, Description, side.
"wm_major_tracts"

#' @title wmh_evidence
#' @docType data
#' @description A data frame describing WMH evidence features.
#' @format A data frame with columns: Value, Description.
"wmh_evidence"

#' @title ppmi_yeo_labels
#' @docType data
#' @description A data frame providing coordinates and names for the PPMI 500 parcels Yeo atlas.
#' @format A data frame with columns: x, y, z, ROI, SystemName, AAL.
"ppmi_yeo_labels"


#' CIT168 reinforcement learning atlas label descriptions
#' @docType data
#' @name CIT168_Reinf_Learn_v1_label_descriptions_pad
NULL

#' CIT168 T1w 700um pad adni brainstem
#' @docType data
#' @name CIT168_T1w_700um_pad_adni_brainstem
NULL

#' FA JHU labels edited
#' @docType data
#' @name FA_JHU_labels_edited
NULL

#' MD JHU labels edited
#' @docType data
#' @name MD_JHU_labels_edited
NULL

#' T template0 LobesBstem
#' @docType data
#' @name T_template0_LobesBstem
NULL

#' cerebellum
#' @docType data
#' @name cerebellum
NULL

#' dkt
#' @docType data
#' @name dkt
NULL

#' dkt cortex cit deep brain
#' @docType data
#' @name dkt_cortex_cit_deep_brain
NULL

#' dlbs
#' @docType data
#' @name dlbs
NULL

#' hemisphere
#' @docType data
#' @name hemisphere
NULL

#' mtl description
#' @docType data
#' @name mtl_description
NULL

#' nbm3CH13
#' @docType data
#' @name nbm3CH13
NULL

#' powers mni itk
#' @docType data
#' @name powers_mni_itk
NULL

#' ppmi template 500Parcels Yeo2011 17Networks 2023 homotopic
#' @docType data
#' @name ppmi_template_500Parcels_Yeo2011_17Networks_2023_homotopic
NULL

#' refbasis brain
#' @docType data
#' @name refbasis_brain
NULL

#' refbasis head
#' @docType data
#' @name refbasis_head
NULL

#' reference basis
#' @docType data
#' @name reference_basis
NULL

#' softwareVersions
#' @docType data
#' @name softwareVersions
NULL
