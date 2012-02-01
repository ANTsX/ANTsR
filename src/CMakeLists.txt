cmake_minimum_required(VERSION 2.8)

project(Ritk)

# find and itk library; create shared object that interacts with it and link against it
find_package(ITK REQUIRED)
include( ${ITK_USE_FILE} )

# headers supplied with the package
include_directories( ${Ritk_SOURCE_DIR}/include )

# find the headers and use them as include directories; use additional paths to search
find_path( R_INCLUDE_DIRS R.h PATHS /usr/share/R/include DOC "path to R include files" )
find_path( RCPP_INCLUDE_DIRS Rcpp.h PATHS /usr/local/lib/R/site-library/Rcpp/include DOC "path to Rcpp include files" )
include_directories( ${RCPP_INCLUDE_DIRS} ${R_INCLUDE_DIRS} )

# directories for R and Rcpp libraries; try to find them at known places
find_library( R_LIB_DIRS R PATHS "/usr/lib/R/lib" DOC "path to R library files" )
find_library( RCPP_LIB_DIRS Rcpp PATHS "/usr/local/lib/R/site-library/Rcpp/lib" DOC "path to Rcpp library files" )
# strip the file-names if any and retain only the path to the library
if( NOT IS_DIRECTORY ${RCPP_LIB_DIRS} )
  get_filename_component( RCPP_LIB_DIRS ${RCPP_LIB_DIRS} PATH )
endif( NOT IS_DIRECTORY ${RCPP_LIB_DIRS} )
if( NOT IS_DIRECTORY ${R_LIB_DIRS} )
  get_filename_component( R_LIB_DIRS ${R_LIB_DIRS} PATH )
endif( NOT IS_DIRECTORY ${R_LIB_DIRS} )
link_directories( ${RCPP_LIB_DIRS} ${R_LIB_DIRS} )

add_library( Ritk SHARED ${Ritk_SOURCE_DIR}/src/antsRegistration.cxx ${Ritk_SOURCE_DIR}/src/antsCommandLineParser.cxx ${Ritk_SOURCE_DIR}/src/antsCommandLineOption.cxx )
target_link_libraries( Ritk Rcpp R ${ITK_LIBRARIES} )
set_target_properties( Ritk PROPERTIES INSTALL_RPATH_USE_PATH TRUE PREFIX "" )
