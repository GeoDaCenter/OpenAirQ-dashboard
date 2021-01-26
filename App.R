library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dashboardthemes)
library(leaflet)
library(rgdal)
library(sf)
library(lubridate)
library(grDevices)
library(plotly)
library(data.table)
library(raster)
library(scales)
library(aqsr)
library(tidyverse)
library(tmap)

##### Data Wrangling #####

## Import PM2.5 Data

import_pm <- function(state_file){
  read_csv(state_file)}

pm2.5 <- dir("Data/PM2.5_2020_12_2021_1", pattern = "\\.csv$", full.names = TRUE)

full.data <- map_df(pm2.5, import_pm)


## Data Cleaning 

county.pm25<- full.data%>%
  filter(COUNTY %in% counties$COUNTYNAME)%>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))%>%
  filter(Date>= '2020-12-01')

county.pm25<- county.pm25%>%
  group_by(`Site ID`)%>%
  summarise(`PM 2.5 emissions` = mean(`Daily Mean PM2.5 Concentration`),
            "SITE_LONGITUDE" = mean(SITE_LONGITUDE),
            "SITE_LATITUDE" = mean(SITE_LATITUDE))
  

## Create a spatial object for sites 

coordinates(county.pm25) <- county.pm25[,c("SITE_LONGITUDE","SITE_LATITUDE")]

proj4string(county.pm25) <- CRS("+init=epsg:4326")

plot(county.pm25)


## Load shapefiles for 21 counties 

counties <- st_read("Data/LargeAreaCounties/LargeAreaCounties.shp")

## Visualization
tmap_mode("view")

tm_shape(counties) +
  tm_borders() +
  tm_shape(county.pm25) +
  tm_bubbles(col  = "PM 2.5 emissions",
             alpha = 0.3,
             size = "PM 2.5 emissions",
             style = "fisher")


