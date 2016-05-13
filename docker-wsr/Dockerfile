FROM rocker/hadleyverse:latest

MAINTAINER Jerad Acosta "jsacosta1103@gmail.com"

# shiny server code

RUN apt-get update && apt-get install -y -t unstable \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev/unstable \
    libxt-dev

# Download and install shiny server
RUN wget --no-verbose https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb && \
    R -e "install.packages(c('shiny', 'rmarkdown'), repos='https://cran.rstudio.com/')" && \
    cp -R /usr/local/lib/R/site-library/shiny/examples/* /srv/shiny-server/

# Setup working environment

# Install Database related apt
RUN apt-get update && apt-get install -y \
    libaio1

# CRAN version packages

# install2.r is an cmd of linux by import litter. If the package is not available in CRAN, it will stop.

RUN install2.r --error \
    assertthat \
    dendextend \
    jsonlite \
    knitr \
    leaflet \
    lubridate \
    openxlsx \
    rmarkdown \
    shinythemes \
    shinydashboard \
    stringr \
    V8 \
  && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

# Github version packages
RUN R -e "devtools::install_github('rstudio/DT')"
RUN R -e "devtools::install_github('rstudio/shiny')"
RUN R -e "devtools::install_github('daattali/shinyjs')"
RUN R -e "devtools::install_github('hadley/httr')"
RUN R -e "devtools::install_github('hadley/readr')"
RUN R -e "devtools::install_github('hadley/dplyr')"
RUN R -e "devtools::install_github('hadley/xml2')"
RUN R -e "devtools::install_github('jennybc/googlesheets')"

COPY shiny-server.conf  /etc/shiny-server/shiny-server.conf
COPY /myapp/* /srv/shiny-server/

EXPOSE 80

COPY shiny-server.sh /usr/bin/shiny-server.sh

CMD ["/usr/bin/shiny-server.sh"]
