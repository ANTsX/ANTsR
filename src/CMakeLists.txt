cmake_minimum_required(VERSION 2.8)

project(ANTsR_SuperBuild)
include( ExternalProject )

if( NOT ITK_DIR )
  ExternalProject_Add( ITK
    TMP_DIR ${ANTsR_SuperBuild_SOURCE_DIR}/ITK/tmp 
    STAMP_DIR ${ANTsR_SuperBuild_SOURCE_DIR}/ITK/stamp
    DOWNLOAD_DIR ${ANTsR_SuperBuild_SOURCE_DIR}/ITK-src
    GIT_REPOSITORY git://itk.org/ITK.git 
    GIT_TAG 93dbbda7a71e18a8bf747657c79dbaa9b27a7092 # using this commit because it antsRegistration works fine with it
    SOURCE_DIR ${ANTsR_SuperBuild_SOURCE_DIR}/ITK/ITK-src
    CMAKE_ARGS -D ITK_BUILD_ALL_MODULES=ON -D CMAKE_BUILD_TYPE=Release -D BUILD_SHARED_LIBS=ON -D BUILD_TESTING=OFF
    BINARY_DIR ${ANTsR_SuperBuild_SOURCE_DIR}/ITK/ITK-build
    INSTALL_COMMAND "" ) # have to include a blank install command or it will try to install into standard dirs
  set( ITK_DIR ${ANTsR_SuperBuild_SOURCE_DIR}/ITK/ITK-build )
endif( NOT ITK_DIR )


ExternalProject_Add( ANTsR 
  #  DEPENDS ITK
  TMP_DIR ${ANTsR_SuperBuild_SOURCE_DIR}/ANTsR/tmp
  STAMP_DIR ${ANTsR_SuperBuild_SOURCE_DIR}/ANTsR/stamp
  SOURCE_DIR ${ANTsR_SuperBuild_SOURCE_DIR}/ANTsR/ANTsR-src
  CMAKE_ARGS -D ITK_DIR=${ITK_DIR} -D RCPP_INCLUDE_DIR=${RCPP_INCLUDE_DIR} -D RCPP_LIB_DIR=${RCPP_LIB_DIR}
             -D R_INCLUDE_DIR=${R_INCLUDE_DIR} -D R_LIB_DIR=${R_LIB_DIR} -D CMAKE_INSTALL_PREFIX=${ANTsR_SuperBuild_SOURCE_DIR}
  BINARY_DIR ${ANTsR_SuperBuild_SOURCE_DIR}/ANTsR/ANTsR-build 
  INSTALL_DIR "" ) # have to include a blank install command or it will try to install into standard dirs
