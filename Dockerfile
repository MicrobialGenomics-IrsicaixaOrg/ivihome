FROM rocker/shiny-verse:latest

## copy repo
RUN rm -rf /srv/shiny-server/*
ADD ./mistralDP /srv/shiny-server
RUN chmod -R +r /srv/shiny-server

## Install deps and packag
RUN Rscript -e "BiocManager::install(update = TRUE, ask=FALSE)"
RUN Rscript -e "devtools::install(dependencies=TRUE, build_vignettes=TRUE, repos = BiocManager::repositories())"

## Expose port
EXPOSE 3838
