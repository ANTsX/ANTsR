FROM rocker/r-base:4.0.5

COPY . /opt/src/ANTsR

# Dependencies option. Defaults to "TRUE" for everything
# Set to NA to get a smaller set of essentials
ARG antsr_dependencies=TRUE

# Install dependencies for devtools install
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
          git \
          wget && \
    wget https://github.com/Kitware/CMake/releases/download/v3.25.3/cmake-3.25.3-linux-x86_64.sh && \
    chmod u+x cmake-3.25.3-linux-x86_64.sh && \
      mkdir -p /opt/cmake-3.25.3 && \
      ./cmake-3.25.3-linux-x86_64.sh --skip-license --prefix=/opt/cmake-3.25.3 && \
      rm ./cmake-3.25.3-linux-x86_64.sh && \
      ln -s /opt/cmake-3.25.3/bin/* /usr/local/bin && \
    R_REMOTES_NO_ERRORS_FROM_WARNINGS=true Rscript -e "install.packages(\"remotes\", repos = \"https://cloud.r-project.org\", dependencies = T)" \
        -e "remotes::install_local(\"/opt/src/ANTsR\", dependencies=${antsr_dependencies})"

ENV R_DEFAULT_PACKAGES="datasets,utils,grDevices,graphics,stats,methods,ANTsR"
ENV ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS=1

LABEL maintainer="ANTsX team (https://github.com/ANTsX)" \
      description="Container for ANTsR" \
      antsr_repo="https://github.com/ANTsX/ANTsR"

ENTRYPOINT ["/usr/local/bin/R"]
