AC_INIT([ANTSR], 0.6) 
m4_include([src/m4/ax_cxx_compile_stdcxx_11.m4])
m4_include([src/m4/vcl_cxx_static_const_init_float.m4])

#!/bin/bash#
RCPP_INCLUDES=`${R_DIR}Rscript -e "cat(system.file(package='Rcpp'))"`/include/
myantssource=${PWD}/src/ants/
CXX_STD=CXX11
CXX=$(R CMD config CXX) 
CXXFLAGS="$(R CMD config CXX1XSTD) $(R CMD config CXX1XFLAGS)"

#CXXFLAGS='${CXXFLAGS} ${CPPFLAGS}'
ITKRCMAKE=`${R_DIR}Rscript -e 'a<-ITKR:::itkIncludes(); cat(a)'`
ITKRLIB=`${R_DIR}Rscript -e 'a<-ITKR:::itkLibs(); cat(a)'`
compflags=`${R_DIR}Rscript -e 'a<-ITKR:::itkCompileFlags(); cat(a)'`
ITKDIR=`${R_DIR}Rscript -e 'a<-ITKR:::itkDir(); cat(a)'`
ITKINCLUDES=`${R_DIR}Rscript -e 'ITKR:::itkIncludes()'`
CPPFLAGS="$(R CMD config CPPFLAGS) -I${PWD}/src/ -I${PWD}/src/ants/ -I${myantssource}Examples/include/ \
  -I${myantssource}/Utilities/ -I${myantssource}/Examples/ \
  -I${myantssource}/Tensor/ -I${RCPP_INCLUDES} -DVCL_STATIC_CONST_INIT_FLOAT=1 \
  -I${ITKINCLUDES}/ -I${R_INCLUDE_DIR} "
echo $CPPFLAGS

# get a version of cmake
cmaker=`which cmake`
if [[ ! -x $cmaker ]] ; then # try r version
  cmaker=`${R_HOME}/bin/Rscript -e 'a<-cmaker::cmake()'`
fi
if [[ $(uname) == "Darwin" ]] ; then
  CMAKE_BUILD_TYPE=Release
fi
if [[  -z $TRAVIS ]];then
    TRAVIS=false;
fi
if [[ $TRAVIS == true ]] ; then
  CMAKE_BUILD_TYPE=Release
fi
cd ./src
antsgit=https://github.com/stnava/ANTs.git
antstag=b04b74a2f10f966fbd4fd642fa0412fe7143bdc6 # mask bug
if [[ ! -s ants/CMakeLists.txt  ]] ; then
    git clone $antsbranch $antsgit ants
fi
cd ants
if [[ -d .git ]]; then
    git checkout master;  git pull;  git checkout $antstag
fi
cd ../
if [[ ! -s antb ]] ; then
  mkdir antb
fi
cd ./antb
${cmaker} -DITK_DIR:PATH=${ITKDIR} \
    -DCMAKE_C_FLAGS="${CMAKE_C_FLAGS} ${compflags} -DNDEBUG  "\
    -DCMAKE_CXX_FLAGS="${CMAKE_CXX_FLAGS} ${compflags} -DNDEBUG  "\
    -DBUILD_SHARED_LIBS=OFF \
    -DBUILD_EXTERNAL_APPLICATIONS=OFF \
    -DBUILD_ALL_ANTS_APPS=OFF \
    -DUSE_SYSTEM_ITK=OFF \
    -DBUILD_TESTING=OFF \
    -DCOPY_SCRIPT_FILES_TO_BIN_DIR=OFF \
    -DCMAKE_BUILD_TYPE:STRING="${CMAKE_BUILD_TYPE}"  ../ants

cd ../..
#  for x in invariantImageSimilarity.cpp ../R/invariantImageSimilarity.R ../R/convolveImage.R ../man/convolveImage.Rd ../man/invariantImageSimilarity.Rd ; do
#    if [[ -s $x ]] ; then  rm $x; fi
#  done


AC_LANG_CPLUSPLUS
VCL_CXX_STATIC_CONST_INIT_FLOAT
AC_SUBST(VCL_STATIC_CONST_INIT_FLOAT)

AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT
cd ./src
