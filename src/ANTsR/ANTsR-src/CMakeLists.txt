cmake_minimum_required(VERSION 2.8)

project(ANTsR)

# set debug build flags
set( CMAKE_CXX_FLAGS_DEBUG "-g -Wall" )
set( CMAKE_C_FLAGS_DEBUG "-g -Wall" )

if( NOT R_INCLUDE_DIR )
  message( FATAL_ERROR "R_INCLUDE_DIR invalid. Perhaps R was not built as library." )
endif( NOT R_INCLUDE_DIR )
if( NOT RCPP_INCLUDE_DIR )
  message( FATAL_ERROR "RCPP_INCLUDE_DIR invalid. Perhaps Rcpp has not been installed." )
endif( NOT RCPP_INCLUDE_DIR )
if( NOT R_LIB_DIR )
  message( FATAL_ERROR "R_LIB_DIR invalid. Perhaps R was not built as library." )
endif( NOT R_LIB_DIR )
if( NOT RCPP_LIB_DIR )
  message( FATAL_ERROR "RCPP_LIB_DIR invalid. Perhaps Rcpp has not been installed." )
endif( NOT RCPP_LIB_DIR )

# extract the paths from the variables supplied for rcpp and r include and lib
string( REGEX MATCHALL "(^-I[^ ]+ *)|( -I[^ ]+ *)" RCPP_INCLUDE_DIR_MATCHES ${RCPP_INCLUDE_DIR} )
string( REGEX MATCHALL "(^-I[^ ]+ *)|( -I[^ ]+ *)" R_INCLUDE_DIR_MATCHES ${R_INCLUDE_DIR} )

foreach( loop_var IN LISTS RCPP_INCLUDE_DIR_MATCHES )
  string( REGEX REPLACE "-I([^ ]+)" "\\1" RCPP_INCLUDE_DIR ${loop_var} )
  include_directories( ${RCPP_INCLUDE_DIR} )
endforeach( loop_var )
 
foreach( loop_var IN LISTS R_INCLUDE_DIR_MATCHES )
  string( REGEX REPLACE "-I([^ ]+)" "\\1" R_INCLUDE_DIR ${loop_var} )
  include_directories( ${R_INCLUDE_DIR} )
endforeach( loop_var )

# ANTS
include_directories( ${ANTS_DIR}/include )
link_directories( ${ANTS_DIR}/lib )

add_library( ANTsR MODULE 
  ${ANTsR_SOURCE_DIR}/src/antsApplyTransforms.cxx
  ${ANTsR_SOURCE_DIR}/src/ANTSIntegrateVectorField.cxx
  ${ANTsR_SOURCE_DIR}/src/ANTSIntegrateVelocityField.cxx
  ${ANTsR_SOURCE_DIR}/src/ANTSJacobian.cxx
  ${ANTsR_SOURCE_DIR}/src/antsMotionCorr.cxx
  ${ANTsR_SOURCE_DIR}/src/antsRegistration.cxx
  ${ANTsR_SOURCE_DIR}/src/ANTS.cxx
  ${ANTsR_SOURCE_DIR}/src/ANTSUseDeformationFieldToGetAffineTransform.cxx
  ${ANTsR_SOURCE_DIR}/src/ANTSUseLandmarkImagesToGetAffineTransform.cxx
  ${ANTsR_SOURCE_DIR}/src/AtroposMin.cxx
  ${ANTsR_SOURCE_DIR}/src/Atropos.cxx
  ${ANTsR_SOURCE_DIR}/src/AverageAffineTransform.cxx
  ${ANTsR_SOURCE_DIR}/src/AverageImages.cxx
  ${ANTsR_SOURCE_DIR}/src/AverageTensorImages.cxx
  ${ANTsR_SOURCE_DIR}/src/CheckTopology.cxx
  ${ANTsR_SOURCE_DIR}/src/ClusterImageStatistics.cxx
  ${ANTsR_SOURCE_DIR}/src/ComposeMultiTransform.cxx
  ${ANTsR_SOURCE_DIR}/src/ComputeSimilarityMetric.cxx
  ${ANTsR_SOURCE_DIR}/src/ConvertImagePixelType.cxx
  ${ANTsR_SOURCE_DIR}/src/ConvertScalarImageToRGB.cxx
  ${ANTsR_SOURCE_DIR}/src/ConvertToJpg.cxx
  ${ANTsR_SOURCE_DIR}/src/ConvertVectorFieldToVTK.cxx
  ${ANTsR_SOURCE_DIR}/src/CopyImageHeaderInformation.cxx
  ${ANTsR_SOURCE_DIR}/src/CreateDisplacementField.cxx
  ${ANTsR_SOURCE_DIR}/src/CreateDTICohort.cxx
  ${ANTsR_SOURCE_DIR}/src/CreateImage.cxx
  ${ANTsR_SOURCE_DIR}/src/CreateWarpedGridImage.cxx
  ${ANTsR_SOURCE_DIR}/src/ExtractRegionFromImageByMask.cxx
  ${ANTsR_SOURCE_DIR}/src/ExtractRegionFromImage.cxx
  ${ANTsR_SOURCE_DIR}/src/ExtractSliceFromImage.cxx
  ${ANTsR_SOURCE_DIR}/src/GetMeshAndTopology.cxx
  ${ANTsR_SOURCE_DIR}/src/ImageCompare.cxx
  ${ANTsR_SOURCE_DIR}/src/ImageMath.cxx
  ${ANTsR_SOURCE_DIR}/src/ImageSetStatistics.cxx
  ${ANTsR_SOURCE_DIR}/src/KellyKapowski.cxx
  ${ANTsR_SOURCE_DIR}/src/KellySlater.cxx
  ${ANTsR_SOURCE_DIR}/src/LabelClustersUniquely.cxx
  ${ANTsR_SOURCE_DIR}/src/LabelGeometryMeasures.cxx
  ${ANTsR_SOURCE_DIR}/src/LabelOverlapMeasures.cxx
  ${ANTsR_SOURCE_DIR}/src/LaplacianThickness.cxx
  ${ANTsR_SOURCE_DIR}/src/MeasureImageSimilarity.cxx
  ${ANTsR_SOURCE_DIR}/src/MeasureMinMaxMean.cxx
  ${ANTsR_SOURCE_DIR}/src/MemoryTest.cxx
  ${ANTsR_SOURCE_DIR}/src/MultiplyImages.cxx
  ${ANTsR_SOURCE_DIR}/src/N3BiasFieldCorrection.cxx
  ${ANTsR_SOURCE_DIR}/src/N4BiasFieldCorrection.cxx
  ${ANTsR_SOURCE_DIR}/src/PasteImageIntoImage.cxx
  ${ANTsR_SOURCE_DIR}/src/PermuteFlipImageOrientationAxes.cxx
  ${ANTsR_SOURCE_DIR}/src/PrintHeader.cxx
  ${ANTsR_SOURCE_DIR}/src/ReorientTensorImage.cxx
  ${ANTsR_SOURCE_DIR}/src/ResampleImageBySpacing.cxx
  ${ANTsR_SOURCE_DIR}/src/ResampleImage.cxx
  ${ANTsR_SOURCE_DIR}/src/ResetDirection.cxx
  ${ANTsR_SOURCE_DIR}/src/sccan.cxx
  ${ANTsR_SOURCE_DIR}/src/SetDirectionByMatrix.cxx
  ${ANTsR_SOURCE_DIR}/src/SetOrigin.cxx
  ${ANTsR_SOURCE_DIR}/src/SetSpacing.cxx
  ${ANTsR_SOURCE_DIR}/src/SmoothImage.cxx
  ${ANTsR_SOURCE_DIR}/src/StackSlices.cxx
  ${ANTsR_SOURCE_DIR}/src/StudentsTestOnImages.cxx
  ${ANTsR_SOURCE_DIR}/src/SurfaceBasedSmoothing.cxx
  ${ANTsR_SOURCE_DIR}/src/SurfaceCurvature.cxx
  ${ANTsR_SOURCE_DIR}/src/TensorDerivedImage.cxx
  ${ANTsR_SOURCE_DIR}/src/ThresholdImage.cxx
  ${ANTsR_SOURCE_DIR}/src/TileImages.cxx
  ${ANTsR_SOURCE_DIR}/src/WarpImageMultiTransform.cxx
  ${ANTsR_SOURCE_DIR}/src/WarpTensorImageMultiTransform.cxx
  ${ANTsR_SOURCE_DIR}/src/WarpTimeSeriesImageMultiTransform.cxx
  ${ANTsR_SOURCE_DIR}/src/WarpVTKPolyDataMultiTransform.cxx
  )

target_link_libraries( ANTsR 
  ${RCPP_LIB_DIR} 
  ${R_LIB_DIR}
  antsApplyTransforms
  ANTSIntegrateVectorField
  ANTSIntegrateVelocityField
  ANTSJacobian
  antsMotionCorr
  antsRegistration
  ANTS
  ANTSUseDeformationFieldToGetAffineTransform
  ANTSUseLandmarkImagesToGetAffineTransform
  AtroposMin
  Atropos
  AverageAffineTransform
  AverageImages
  AverageTensorImages
  CheckTopology
  ClusterImageStatistics
  ComposeMultiTransform
  ComputeSimilarityMetric
  ConvertImagePixelType
  ConvertScalarImageToRGB
  ConvertToJpg
  ConvertVectorFieldToVTK
  CopyImageHeaderInformation
  CreateDisplacementField
  CreateDTICohort
  CreateImage
  CreateWarpedGridImage
  ExtractRegionFromImageByMask
  ExtractRegionFromImage
  ExtractSliceFromImage
  GetMeshAndTopology
  ImageCompare
  ImageMath
  ImageSetStatistics
  KellyKapowski
  KellySlater
  LabelClustersUniquely
  LabelGeometryMeasures
  LabelOverlapMeasures
  LaplacianThickness
  MeasureImageSimilarity
  MeasureMinMaxMean
  MemoryTest
  MultiplyImages
  N3BiasFieldCorrection
  N4BiasFieldCorrection
  PasteImageIntoImage
  PermuteFlipImageOrientationAxes
  PrintHeader
  ReorientTensorImage
  ResampleImageBySpacing
  ResampleImage
  ResetDirection
  sccan
  SetDirectionByMatrix
  SetOrigin
  SetSpacing
  SmoothImage
  StackSlices
  StudentsTestOnImages
  SurfaceBasedSmoothing
  SurfaceCurvature
  TensorDerivedImage
  ThresholdImage
  TileImages
  WarpImageMultiTransform
  WarpTensorImageMultiTransform
  WarpTimeSeriesImageMultiTransform
  WarpVTKPolyDataMultiTransform
  )
set_target_properties( ANTsR PROPERTIES INSTALL_RPATH_USE_LINK_PATH TRUE PREFIX "" )
install( TARGETS ANTsR DESTINATION . )
