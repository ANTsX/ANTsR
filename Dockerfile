FROM rocker/tidyverse:4.2.3 AS builder

COPY . /opt/src/ANTsR

WORKDIR /opt/src

RUN git clone --depth 1 https://github.com/stnava/ITKR.git && \
    git clone --depth 1 https://github.com/ANTsX/ANTsRCore.git

# Get CMake
RUN wget https://github.com/Kitware/CMake/releases/download/v3.25.3/cmake-3.25.3-linux-x86_64.sh && \
    chmod u+x cmake-3.25.3-linux-x86_64.sh && \
    ./cmake-3.25.3-linux-x86_64.sh --skip-license --prefix=/usr/local && \
    rm ./cmake-3.25.3-linux-x86_64.sh

# Minimal dependencies, will not enable all functionality
# Some of these already exist in the rocker image, but install
# anyway in case base image changes
RUN R_REMOTES_NO_ERRORS_FROM_WARNINGS=true \
      Rscript -e "install.packages( \
                   c('Rcpp', \
                     'RcppEigen', \
                     'magrittr',  \
                     'rsvd',      \
                     'magic',     \
                     'psych'), dependencies = TRUE )"

RUN R CMD INSTALL ITKR && \
    R CMD INSTALL ANTsRCore && \
    R CMD INSTALL ANTsR

# Some packages were removed from CRAN and need a github install
RUN R_REMOTES_NO_ERRORS_FROM_WARNINGS=true \
      Rscript -e "library(devtools)" \
              -e "install_github('cran/ifultools')" \
              -e "install_github('cran/DMwR')"

RUN R_REMOTES_NO_ERRORS_FROM_WARNINGS=true \
      Rscript -e "\
      install.packages(c( \
        'abind', \
        'BGLR', \
        'caret', \
        'cluster', \
        'corpcor', \
        'dplyr', \
        'e1071', \
        'extremevalues', \
        'fastICA', \
        'FNN', \
        'fpc', \
        'ggplot2', \
        'glasso', \
        'glmnet', \
        'grid', \
        'hdf5r', \
        'igraph', \
        'irlba', \
        'knitr', \
        'mFilter', \
        'misc3d', \
        'moments', \
        'networkD3', \
        'pixmap', \
        'png', \
        'pracma', \
        'R.matlab', \
        'randomForest', \
        'RcppHNSW', \
        'rgl', \
        'rmarkdown', \
        'robust', \
        'robustbase', \
        'signal', \
        'sna', \
        'testthat', \
        'viridis', \
        'visreg', \
        'wmtsa', \
        'xgboost' \
     ), repos = 'https://cloud.r-project.org')"

# Additional dependencies
# Derived from remotes::local_package_deps
FROM rocker/tidyverse:4.2.3 AS runtime

COPY --from=builder /usr/local/lib/R/site-library/ /usr/local/lib/R/site-library/

ENV R_DEFAULT_PACKAGES="datasets,utils,grDevices,graphics,stats,methods,ANTsR"
ENV ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS=1

WORKDIR /

CMD ["R"]
