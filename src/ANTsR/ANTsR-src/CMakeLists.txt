cmake_minimum_required(VERSION 2.8.7)

project(ANTsR)

# set debug build flags
set( CMAKE_CXX_FLAGS_DEBUG "-g -Wall" CACHE STRING "Debug flags" FORCE )
set( CMAKE_C_FLAGS_DEBUG "-g -Wall" CACHE STRING "Debug flags" FORCE )

# check if any of the following required directories are not set; 
if( NOT R_INCLUDE_DIR )
  message( FATAL_ERROR "R_INCLUDE_DIR invalid. Perhaps R was not built as library. Please Re-build R with '--enable-R-shlib'" )
endif( NOT R_INCLUDE_DIR )
if( NOT RCPP_INCLUDE_DIR )
  message( FATAL_ERROR "RCPP_INCLUDE_DIR invalid. Perhaps Rcpp has not been installed." )
endif( NOT RCPP_INCLUDE_DIR )
if( NOT R_LIB_DIR )
  message( FATAL_ERROR "R_LIB_DIR invalid. Perhaps R was not built as library. Please Re-build R with '--enable-R-shlib'" )
endif( NOT R_LIB_DIR )
if( NOT RCPP_LIB_DIR )
  message( FATAL_ERROR "RCPP_LIB_DIR invalid. Perhaps Rcpp has not been installed." )
endif( NOT RCPP_LIB_DIR )

# extract the paths from the variables supplied for rcpp and r include and lib; the paths include the flags to compiler and linker
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

# ITK ; ITK_DIR is set by caller
find_package( ITK REQUIRED )
include( ${ITK_USE_FILE} )

# get_cmake_property(_variableNames VARIABLES)
# foreach (_variableName ${_variableNames})
#   message(STATUS "${_variableName}=${${_variableName}}")
# endforeach()

# ANTS ; ANTS_DIR is set by caller
include_directories( ${ANTS_DIR}/include )
include_directories( ${ANTS_INC} )
link_directories( ${ANTS_DIR}/lib )

# all targets must have rpath installed so R can find the dependencies when loading them
# set( CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE )

# ANTsR targets
add_library(RantsApplyTransforms MODULE ${ANTsR_SOURCE_DIR}/src/antsApplyTransforms.cxx)
target_link_libraries(RantsApplyTransforms ${RCPP_LIB_DIR} ${R_LIB_DIR} l_antsApplyTransforms )
install( TARGETS RantsApplyTransforms    DESTINATION  .  )

add_library(RantsMotionCorr MODULE ${ANTsR_SOURCE_DIR}/src/antsMotionCorr.cxx)
target_link_libraries(RantsMotionCorr ${RCPP_LIB_DIR} ${R_LIB_DIR} l_antsMotionCorr )
install( TARGETS RantsMotionCorr    DESTINATION  .  )

add_library(RantsRegistration MODULE ${ANTsR_SOURCE_DIR}/src/antsRegistration.cxx)
target_link_libraries(RantsRegistration ${RCPP_LIB_DIR} ${R_LIB_DIR} l_antsRegistration )
install( TARGETS RantsRegistration    DESTINATION  .  )

add_library(RantsAffineInitializer MODULE ${ANTsR_SOURCE_DIR}/src/antsAffineInitializer.cxx)
target_link_libraries(RantsAffineInitializer ${RCPP_LIB_DIR} ${R_LIB_DIR} l_antsAffineInitializer )
install( TARGETS RantsAffineInitializer    DESTINATION  .  )

add_library(RAtropos MODULE ${ANTsR_SOURCE_DIR}/src/Atropos.cxx)
target_link_libraries(RAtropos ${RCPP_LIB_DIR} ${R_LIB_DIR} l_Atropos )
install( TARGETS RAtropos    DESTINATION  .  )

add_library(RImageMath MODULE ${ANTsR_SOURCE_DIR}/src/ImageMath.cxx)
target_link_libraries(RImageMath ${RCPP_LIB_DIR} ${R_LIB_DIR} l_ImageMath )
install( TARGETS RImageMath    DESTINATION  .  )

add_library(RKellyKapowski MODULE ${ANTsR_SOURCE_DIR}/src/KellyKapowski.cxx)
target_link_libraries(RKellyKapowski ${RCPP_LIB_DIR} ${R_LIB_DIR} l_KellyKapowski )
install( TARGETS RKellyKapowski    DESTINATION  .  )

add_library(RLabelClustersUniquely MODULE ${ANTsR_SOURCE_DIR}/src/LabelClustersUniquely.cxx)
target_link_libraries(RLabelClustersUniquely ${RCPP_LIB_DIR} ${R_LIB_DIR} l_LabelClustersUniquely )
install( TARGETS RLabelClustersUniquely    DESTINATION  .  )

add_library(RN3BiasFieldCorrection MODULE ${ANTsR_SOURCE_DIR}/src/N3BiasFieldCorrection.cxx)
target_link_libraries(RN3BiasFieldCorrection ${RCPP_LIB_DIR} ${R_LIB_DIR} l_N3BiasFieldCorrection )
install( TARGETS RN3BiasFieldCorrection    DESTINATION  .  )

add_library(RN4BiasFieldCorrection MODULE ${ANTsR_SOURCE_DIR}/src/N4BiasFieldCorrection.cxx)
target_link_libraries(RN4BiasFieldCorrection ${RCPP_LIB_DIR} ${R_LIB_DIR} l_N4BiasFieldCorrection )
install( TARGETS RN4BiasFieldCorrection    DESTINATION  .  )

add_library(RResampleImageBySpacing MODULE ${ANTsR_SOURCE_DIR}/src/ResampleImageBySpacing.cxx)
target_link_libraries(RResampleImageBySpacing ${RCPP_LIB_DIR} ${R_LIB_DIR} l_ResampleImageBySpacing )
install( TARGETS RResampleImageBySpacing    DESTINATION  .  )

add_library(Rsccan MODULE ${ANTsR_SOURCE_DIR}/src/sccan.cxx)
target_link_libraries(Rsccan ${RCPP_LIB_DIR} ${R_LIB_DIR} l_sccan )
install( TARGETS Rsccan    DESTINATION  .  )

add_library(RSmoothImage MODULE ${ANTsR_SOURCE_DIR}/src/SmoothImage.cxx)
target_link_libraries(RSmoothImage ${RCPP_LIB_DIR} ${R_LIB_DIR} l_SmoothImage )
install( TARGETS RSmoothImage    DESTINATION  .  )

add_library(RSurfaceCurvature MODULE ${ANTsR_SOURCE_DIR}/src/SurfaceCurvature.cxx)
target_link_libraries(RSurfaceCurvature ${RCPP_LIB_DIR} ${R_LIB_DIR} l_SurfaceCurvature )
install( TARGETS RSurfaceCurvature    DESTINATION  .  )

add_library(RThresholdImage MODULE ${ANTsR_SOURCE_DIR}/src/ThresholdImage.cxx)
target_link_libraries(RThresholdImage ${RCPP_LIB_DIR} ${R_LIB_DIR} l_ThresholdImage )
install( TARGETS RThresholdImage    DESTINATION  .  )

add_library( RantsImageRead MODULE ${ANTsR_SOURCE_DIR}/src/antsImageRead.cxx )
target_link_libraries( RantsImageRead ${RCPP_LIB_DIR} ${R_LIB_DIR} ${ITK_LIBRARIES} )
install( TARGETS RantsImageRead    DESTINATION  .  )

add_library( RantsImageWrite MODULE ${ANTsR_SOURCE_DIR}/src/antsImageWrite.cxx )
target_link_libraries( RantsImageWrite ${RCPP_LIB_DIR} ${R_LIB_DIR} ${ITK_LIBRARIES} )
install( TARGETS RantsImageWrite    DESTINATION  .  )

add_library( RantsImage MODULE ${ANTsR_SOURCE_DIR}/src/antsImage.cxx )
target_link_libraries( RantsImage ${RCPP_LIB_DIR} ${R_LIB_DIR} ${ITK_LIBRARIES} )
install( TARGETS RantsImage    DESTINATION  .  )

add_library( RantsImageClone MODULE ${ANTsR_SOURCE_DIR}/src/antsImageClone.cxx )
target_link_libraries( RantsImageClone ${RCPP_LIB_DIR} ${R_LIB_DIR} ${ITK_LIBRARIES} )
install( TARGETS RantsImageClone    DESTINATION  .  )

add_library( RantsImageList MODULE ${ANTsR_SOURCE_DIR}/src/antsImageList.cxx )
target_link_libraries( RantsImageList ${RCPP_LIB_DIR} ${R_LIB_DIR} ${ITK_LIBRARIES} )
install( TARGETS RantsImageList    DESTINATION  .  )

add_library( RantsMatrix MODULE ${ANTsR_SOURCE_DIR}/src/antsMatrix.cxx )
target_link_libraries( RantsMatrix ${RCPP_LIB_DIR} ${R_LIB_DIR} ${ITK_LIBRARIES} )
install( TARGETS RantsMatrix    DESTINATION  .  )
