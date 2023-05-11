FROM rocker/tidyverse:4.2.3

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
RUN R_REMOTES_NO_ERRORS_FROM_WARNINGS=true Rscript -e "install.packages( pkgs = c(\"Rcpp\", \"RcppEigen\", \"magrittr\", \"rsvd\", \"magic\", \"psych\"), dependencies = TRUE )"

RUN R CMD INSTALL ITKR && \
    R CMD INSTALL ANTsRCore && \
    R CMD INSTALL ANTsR

ENV R_DEFAULT_PACKAGES="datasets,utils,grDevices,graphics,stats,methods,ANTsR"
ENV ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS=1

WORKDIR /

CMD ["R"]
