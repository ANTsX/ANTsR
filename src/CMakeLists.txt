cmake_minimum_required(VERSION 2.8.7)
project(ANTsR_SuperBuild)
include( ExternalProject )
# if 'CMAKE_BUILD_TYPE' is not specified by user, assume 'Release'
if( NOT CMAKE_BUILD_TYPE )
  set( CMAKE_BUILD_TYPE Release )
endif( NOT CMAKE_BUILD_TYPE )

#   we have to define NDEBUG due to a possible bug in itkMacro.h --- we
#   require   return static_cast<TTarget>(x); to be implemented rather 
#   than the dynamic_cast

# if 'ANTS_DIR' is not specified by user, superbuild ANTS
if( NOT ANTS_DIR )
  ExternalProject_Add( ANTS
#    DEPENDS ${ANTS_DEPENDSON_ITK} # has the value 'ITK' when ITK is superbuilt; is not set otherwise
    TMP_DIR ${ANTsR_SuperBuild_SOURCE_DIR}/ANTS/tmp 
    STAMP_DIR ${ANTsR_SuperBuild_SOURCE_DIR}/ANTS/stamp
    DOWNLOAD_DIR ${ANTsR_SuperBuild_SOURCE_DIR}/ANTS/ANTS-src
    SVN_REPOSITORY http://svn.code.sf.net/p/advants/svn/trunk 
    SVN_REVISION -r 1776
#    GIT_REPOSITORY http://github.com/stnava/ANTS.git 
#    GIT_TAG  19207c855df5258eacd1ec3e6d3abb100fcc93da
   SOURCE_DIR ${ANTsR_SuperBuild_SOURCE_DIR}/ANTS/ANTS-src
   CMAKE_ARGS -D CMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE} # build type set by user
               -D BUILD_SHARED_LIBS=ON # R requires shared objects
	       -D BUILD_TESTING=OFF # reduces build time
	       -D USE_SYSTEM_ITK=OFF # use the ITK built above using superbuild or use the one specified by user through 'ITK_DIR'
 #cmakedefine NDEBUG 
    BINARY_DIR ${ANTsR_SuperBuild_SOURCE_DIR}/ANTS/ANTS-build
    INSTALL_COMMAND "" )
  set( ANTS_DIR ${ANTsR_SuperBuild_SOURCE_DIR}/ANTS/ANTS-build )
  set( ITK_DIR ${ANTsR_SuperBuild_SOURCE_DIR}/ANTS/ANTS-build/ITKv4-build )
  set( ANTsR_DEPENDSON_ANTS ANTS ) # set when ANTS is superbuilt so that ANTS is superbuilt prior to ANTsR
  set( ANTS_INC ${ANTsR_SuperBuild_SOURCE_DIR}/ANTS/ANTS-src/Examples/include )
endif( NOT ANTS_DIR )

# main project itself is superbuilt
ExternalProject_Add( ANTsR 
  DEPENDS ${ANTsR_DEPENDSON_ITK} ${ANTsR_DEPENDSON_ANTS} # has the value 'ANTS' when ANTS is superbuilt; it is not set otherwise
  TMP_DIR ${ANTsR_SuperBuild_SOURCE_DIR}/ANTsR/tmp
  STAMP_DIR ${ANTsR_SuperBuild_SOURCE_DIR}/ANTsR/stamp
  SOURCE_DIR ${ANTsR_SuperBuild_SOURCE_DIR}/ANTsR/ANTsR-src
  CMAKE_ARGS -D ANTS_DIR=${ANTS_DIR} # user specified ANTS-build directory or ANTS-build superbuilt from above
             -D ANTS_INC=${ANTS_INC}  # ANTS include directory 
             -D ITK_DIR=${ITK_DIR} # ANTS ITK-build directory
	     -D RCPP_INCLUDE_DIR=${RCPP_INCLUDE_DIR} # Rcpp include directory
	     -D RCPP_LIB_DIR=${RCPP_LIB_DIR} # Rcpp library directory
             -D R_INCLUDE_DIR=${R_INCLUDE_DIR} # R include directory
	     -D R_LIB_DIR=${R_LIB_DIR} # R library directory
	     -D CMAKE_INSTALL_PREFIX=${ANTsR_SuperBuild_SOURCE_DIR} # all the targets are to be installed to current directory so R can find and load them
	     -D CMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE} # build type set by the user
  #cmakedefine NDEBUG 
  BINARY_DIR ${ANTsR_SuperBuild_SOURCE_DIR}/ANTsR/ANTsR-build 
  INSTALL_DIR "" ) 

message("CMAKE_CXX_FLAGS_RELEASE is ${CMAKE_CXX_FLAGS_RELEASE}")
message("CMAKE_BUILD_TYPE is ${CMAKE_BUILD_TYPE}")
