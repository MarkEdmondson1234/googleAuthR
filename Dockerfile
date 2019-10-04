FROM r-base

COPY . ./

# install R package dependencies
RUN apt-get update && apt-get install -y \
    libssl-dev \
    ## clean up
    && apt-get clean \ 
    && rm -rf /var/lib/apt/lists/ \ 
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds
    
## Install packages from CRAN
RUN install2.r --error \ 
    -r 'http://cran.rstudio.com' \
    googleAuthR \
    ## install Github packages
    installGithub.r MarkEdmondson1234/googleAuthR \
    ## clean up
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds
