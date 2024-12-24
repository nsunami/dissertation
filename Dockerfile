FROM rocker/rstudio:4

# Install Ubuntu distributions of R Packages
RUN apt-get update
RUN apt-get install -y zlib1g-dev
RUN 
RUN install2.r -e here 
RUN install2.r -e haven

# xml2
RUN apt-get install -y libxml2-dev
RUN install2.r -e xml2 

# svg lite
RUN apt-get -y install libfontconfig1-dev
RUN install2.r -e svglite 

RUN install2.r -e patchwork 
RUN install2.r -e table1 
RUN install2.r -e papaja 
RUN install2.r -e kableExtra
RUN install2.r -e tidyr 
RUN install2.r -e tibble 

RUN install2.r -e bookdown


RUN install2.r -e remotes
# ggtext
RUN apt-get -y install libcurl4-openssl-dev
RUN install2.r -e ggtext
RUN install2.r -e labelled

# Install R packages on GitHub
RUN Rscript -e "remotes::install_github('achetverikov/APAstats')"


RUN install2.r -e readr 
RUN install2.r -e stringr
RUN install2.r -e purrr
RUN install2.r -e tinytex
RUN apt-get -y install texlive-publishers texlive-fonts-extra texlive-latex-extra texlive-humanities lmodern
RUN Rscript -e "remotes::install_github('crsh/papaja')"

RUN apt-get -y install libcairo2-dev libharfbuzz-dev libfribidi-dev
RUN apt-get -y install libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev
RUN install2.r -e flextable
RUN install2.r -e metathis
RUN install2.r -e Hmisc
RUN install2.r -e GGally
RUN install2.r -e corrr

RUN apt-get -y install cmake
RUN install2.r -e lme4

RUN install2.r -e lmerTest
RUN install2.r -e forestplot
RUN install2.r -e broom.mixed
RUN install2.r -e ggpubr
RUN install2.r -e pacman

RUN install2.r -e tidyverse
RUN install2.r -e rio

RUN install2.r -e english
RUN install2.r -e sjlabelled
RUN install2.r -e sjPlot
RUN install2.r -e ggh4x
RUN install2.r -e knitcitations