FROM rocker/rstudio:4.5.0

ARG NTERMREPORT_VERSION=0.0.9
# Fixed the messy variable name here
ENV NTERMREPORT_VERSION=$NTERMREPORT_VERSION
LABEL version=$NTERMREPORT_VERSION

LABEL org.opencontainers.image.title="Gevaert-Lab nterminalreport"
LABEL org.opencontainers.image.authors="Andrea Argentini <aargentini@gmail.com>"
LABEL org.opencontainers.image.description="RStudio environment for N-terminal analysis"
LABEL org.opencontainers.image.url="https://github.com/Gevaert-Lab/nterminalreport"

# --------------------------------------------------
# Basic utilities & Chrome dependencies
# --------------------------------------------------
RUN apt-get update && apt-get install -y --fix-missing \
    wget \
    unzip \
    default-jre \
    gdebi-core \
    libfontconfig1 \
    libnss3 \
    libatk-bridge2.0-0 \
    libgtk-3-0 \
    libxss1 \
    libasound2t64 \
    && rm -rf /var/lib/apt/lists/*

# --------------------------------------------------
# Development & Numerical libraries
# --------------------------------------------------
RUN apt-get update && apt-get install -y --fix-missing \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    zlib1g-dev \
    libpng-dev \
    libjpeg-dev \
    libfreetype6-dev \
    libnetcdf-dev \
    libhdf5-dev \
    libglpk40 \
    libglpk-dev \
    libboost-all-dev \
    && rm -rf /var/lib/apt/lists/*

# --------------------------------------------------
# Quarto CLI & Google Chrome
# --------------------------------------------------
RUN wget -qO quarto.deb https://github.com/quarto-dev/quarto-cli/releases/download/v1.8.26/quarto-1.8.26-linux-amd64.deb \
    && apt-get update && apt-get install -y ./quarto.deb && rm quarto.deb

RUN wget -q https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb \
    && apt-get update && apt-get install -y ./google-chrome-stable_current_amd64.deb && rm google-chrome-stable_current_amd64.deb

# --------------------------------------------------
# Environment variables
# --------------------------------------------------
ENV OPENSSL_CONF=/etc/ssl/openssl.cnf
ENV R_LIBS_SITE=/usr/local/lib/R/site-library
ENV CHROMOTE_CHROME=/usr/bin/google-chrome

# --------------------------------------------------
# R packages
# --------------------------------------------------
RUN R -e "install.packages(c('BiocManager','remotes','rlang','openxlsx','tibble','withr','utils','assertthat','fs','DT','tidyr','reactable','stringr','htmltools','purrr','methods','upsetjs','heatmaply','plotly','yaml','logger','glue'), repos='https://cloud.r-project.org', lib='/usr/local/lib/R/site-library'); \
          BiocManager::install(c('QFeatures','msqrob2','MSnbase'), lib='/usr/local/lib/R/site-library'); \
          remotes::install_github('Gevaert-Lab/diareport@v0.9.0', dependencies=TRUE); \ 
          remotes::install_github('Gevaert-Lab/ntermreport@${NTERMREPORT_VERSION}', dependencies=TRUE)"

# --------------------------------------------------
# Verify Installations
# --------------------------------------------------
RUN quarto --version && google-chrome --version