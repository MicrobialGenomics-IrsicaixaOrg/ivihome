FROM rocker/shiny-verse:latest

## copy repo
RUN rm -rf /srv/shiny-server/*
ADD . /srv/shiny-server
RUN chmod -R +r /srv/shiny-server

## Install deps and packag
RUN apt-get update && apt-get install -y libglpk40
RUN Rscript -e "devtools::install('/srv/shiny-server', dependencies=TRUE, build_vignettes=TRUE)"

## Expose port
EXPOSE 3838
