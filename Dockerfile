FROM ubuntu:bionic-20200713

# Default to build latest version
ARG antsr_version="HEAD" 

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
                    apt-transport-https \
                    build-essential \
                    ca-certificates \
                    curl \
                    git \
                    gnupg \
                    libcurl4-gnutls-dev \
                    libssl-dev \
                    libxml2-dev \
                    wget

# add R stuff for apt
# https://cran.r-project.org/bin/linux/ubuntu/README.html
COPY docker/ubuntuRKey.txt /opt

RUN echo "deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran40/" >> /etc/apt/sources.list && \
    apt-key add /opt/ubuntuRKey.txt && \
    apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
                                   r-base \
                                   r-base-dev && \
    wget -O - https://apt.kitware.com/keys/kitware-archive-latest.asc 2>/dev/null | apt-key add - && \
    echo "deb https://apt.kitware.com/ubuntu/ bionic main" >> /etc/apt/sources.list && \
    apt-get update && \
    apt-get -y install cmake && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*


RUN mkdir /opt/src && \
    git clone https://github.com/stnava/ITKR.git /opt/src/ITKR && \
    git clone https://github.com/ANTsX/ANTsRCore.git /opt/src/ANTsRCore 

COPY . /opt/src/ANTsR

RUN cd /opt/src/ITKR && git log | head -n 1 | cut -d ' ' -f 2 > /opt/ITKR_version.txt && \
    cd /opt/src/ANTsRCore && git log | head -n 1 | cut -d ' ' -f 2 > /opt/ANTsRCore_version.txt && \
    cd /opt/src/ANTsR && git log | head -n 1 | cut -d ' ' -f 2 > /opt/ANTsR_version.txt && \
    Rscript /opt/src/ANTsR/docker/installDependencies.R && \
    R CMD INSTALL /opt/src/ITKR && \
    R CMD INSTALL /opt/src/ANTsRCore && \
    R CMD INSTALL /opt/src/ANTsR && \
    rm -rf /opt/src

ENV R_DEFAULT_PACKAGES="datasets,utils,grDevices,graphics,stats,methods,ANTsR"
ENV ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS=1


CMD ["R"]
