FROM rocker/rstudio:4.4.2

# Install renv
RUN R -e "install.packages('renv', repos = c(CRAN = 'https://cloud.r-project.org'))"

# Get the renv files from the current project 
RUN mkdir -p /project
WORKDIR /project
COPY renv.lock renv.lock

# Specify library paths for installation
RUN mkdir -p renv
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json

# Install system dependencies
RUN apt-get update
## xml2
RUN apt-get install -y libxml2-dev
## png
RUN apt-get install -y libpng-dev
## curl
RUN apt-get install -y libcurl4-openssl-dev
## cmake (for nloptr)
RUN apt-get install -y cmake 
## gert
RUN apt-get install -y libgit2-dev 
## systemfonts
RUN apt-get install -y libfontconfig1-dev
## Other dependencies
RUN apt-get install -y libcairo2-dev libharfbuzz-dev libfribidi-dev
RUN apt-get install -y libfreetype6-dev libtiff5-dev libjpeg-dev

# Restore the packages via renv
RUN R -e "renv::restore()"

# Install knitr and rmarkdown packages
RUN R -e "install.packages('knitr', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "install.packages('rmarkdown', repos = c(CRAN = 'https://cloud.r-project.org'))"

# Install tinytex as the rstudio user
RUN Rscript -e "tinytex::install_tinytex(force = TRUE)"