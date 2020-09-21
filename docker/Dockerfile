FROM ubuntu:bionic-20200713

# Default to build latest version
ARG antsr_version="HEAD" 

# Dependencies option. Defaults to "TRUE" for everything
# Set to NA to get a smaller set of essentials
ARG antsr_dependencies=TRUE

RUN apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
                    apt-transport-https \
                    build-essential \
                    ca-certificates \
                    curl \
                    file \
                    git \
                    gnupg \
                    libcurl4-gnutls-dev \
                    libssl-dev \
                    libxml2-dev \
                    tcl \
                    tcl-dev \
                    tk \
                    tk-dev \
                    wget

# add R stuff for apt
# https://cran.r-project.org/bin/linux/ubuntu/README.html
COPY ubuntuRKey.txt /opt

RUN echo "deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran40/" >> /etc/apt/sources.list && \
    apt-key add /opt/ubuntuRKey.txt && \
    apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
                    r-base \
                    r-base-dev && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# The warnings variable is needed to suppress warnings from tcltk that turn into an
# error about not finding DISPLAY
#
# Also suppresses missing dependency errors
RUN R_REMOTES_NO_ERRORS_FROM_WARNINGS=true Rscript \
            -e "install.packages(\"devtools\", repos = \"https://cloud.r-project.org\", dependencies = T)" \
            -e "library(devtools)" \
            -e "install_github(\"stnava/cmaker\")" \
            -e "library(devtools)" \
            -e "install_github(\"cran/ifultools\")" \
            -e "install_github(\"ANTsX/ANTsR\", ref=\"${antsr_version}\", dependencies=${antsr_dependencies})"

ENV R_DEFAULT_PACKAGES="datasets,utils,grDevices,graphics,stats,methods,ANTsR"
ENV ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS=1


CMD ["R"]
