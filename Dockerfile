FROM rocker/shiny:4.0.4
RUN install2.r --error \
    rsconnect \
    shiny \
    shinydashboard \
    shinyWidgets \
    dashboardthemes \
    leaflet \
    rgdal \
    sf \
    lubridate \
    grDevices \
    tidyverse \
    classInt \
    plotly \
    data.table \
    raster \
    scales \
    zoo \
    ggthemes \
    RSocrata \
    plyr \
    covr \
    rex
    
WORKDIR /home/shinyusr
COPY ./ .
CMD Rscript deploy.R