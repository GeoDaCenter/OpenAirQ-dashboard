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
library(tmap)
library(tidyverse)
library(zoo)

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
  select(Date, `Site ID`,`Daily Mean PM2.5 Concentration`, COUNTY,SITE_LATITUDE, SITE_LONGITUDE )%>%
  group_by(Date, `Site ID`,COUNTY )%>%
  summarise(pm2.5 = mean(`Daily Mean PM2.5 Concentration`),
            latitude = mean(SITE_LATITUDE), 
            longitude = mean(SITE_LONGITUDE) )%>%
  pivot_wider(names_from = Date, values_from = "pm2.5")


a<- na.approx(county.pm25, rule = 2)


a<- reshape(county.pm25, timevar = "Date",idvar = 'Site ID', direction = 'wide')  

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

##### DATA LOADING END #####

##### VARIABLE START #####

mapheight = 500


descriptions<- read_csv("Data/Description.csv")

pm25.tabname <- "pm25"
pm25.name <- "Particulate Matter < 2.5μm (PM2.5)"
pm25.description <- descriptions$Description[descriptions["Variable"] == "PM2.5"]
pm25.source <- descriptions$Source[descriptions["Variable"] == "PM2.5"]
##### VARIABLE END #####

##### THEME START #####

chicago_blue <- "rgb(128, 206, 255)"
chicago_red <- "rgb(199, 20, 20)"

sidebar_select_gradient <- cssGradientThreeColors(
  direction = "right"
  ,colorStart = "rgb(255, 67, 67)"
  ,colorMiddle = "rgb(255, 120, 120)"
  ,colorEnd = "rgb(255,175,175)"
  ,colorStartPos = 0
  ,colorMiddlePos = 30
  ,colorEndPos = 100
)

# sidebar_hover_gradient <- cssGradientThreeColors(
#   direction = "right"
#   ,colorStart = chicago_red
#   ,colorMiddle = "rgba(199,80,80,1)"
#   ,colorEnd = "rgba(199,110,110, 1)"
#   ,colorStartPos = 0
#   ,colorMiddlePos = 30
#   ,colorEndPos = 100
# )
sidebar_hover_gradient <- sidebar_select_gradient

### creating custom theme object
theme_air_chicago <- shinyDashboardThemeDIY(
  
  ### general
  appFontFamily = "Arial"
  ,appFontColor = "rgb(0,0,0)"
  ,primaryFontColor = "rgb(0,0,0)"
  ,infoFontColor = "rgb(0,0,0)"
  ,successFontColor = "rgb(0,0,0)"
  ,warningFontColor = "rgb(0,0,0)"
  ,dangerFontColor = "rgb(0,0,0)"
  ,bodyBackColor = "rgb(217,217,217)"
  
  ### header
  ,logoBackColor = chicago_blue
  
  ,headerButtonBackColor = chicago_blue
  ,headerButtonIconColor = "rgb(245,245,245)"
  ,headerButtonBackColorHover = chicago_blue
  ,headerButtonIconColorHover = "rgb(0,0,0)"
  
  ,headerBackColor = chicago_blue
  ,headerBoxShadowColor = "#aaaaaa"
  ,headerBoxShadowSize = "2px 2px 2px"
  
  ### sidebar
  ,sidebarBackColor = chicago_blue
  ,sidebarPadding = 2
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = "3px 5px 5px"
  ,sidebarShadowColor = "#aaaaaa"
  
  ,sidebarUserTextColor = "rgb(255,255,255)"
  
  ,sidebarSearchBackColor = "rgb(55,72,80)"
  ,sidebarSearchIconColor = "rgb(153,153,153)"
  ,sidebarSearchBorderColor = "rgb(55,72,80)"
  
  ,sidebarTabTextColor = "rgb(255,255,255)"
  ,sidebarTabTextSize = 13
  ,sidebarTabBorderStyle = "none none solid none"
  ,sidebarTabBorderColor = "rgb(35,106,135)"
  ,sidebarTabBorderWidth = 1
  
  ,sidebarTabBackColorSelected = sidebar_select_gradient
  ,sidebarTabTextColorSelected = "rgb(0,0,0)"
  ,sidebarTabRadiusSelected = "0px 20px 20px 0px"
  
  ,sidebarTabBackColorHover = sidebar_hover_gradient
  ,sidebarTabTextColorHover = "rgb(50,50,50)"
  ,sidebarTabBorderStyleHover = "none none solid none"
  ,sidebarTabBorderColorHover = "rgb(75,126,151)"
  ,sidebarTabBorderWidthHover = 1
  ,sidebarTabRadiusHover = "0px 20px 20px 0px"
  
  ### boxes
  ,boxBackColor = "rgb(255,255,255)"
  ,boxBorderRadius = 5
  ,boxShadowSize = "0px 1px 1px"
  ,boxShadowColor = "rgba(0,0,0,.1)"
  ,boxTitleSize = 16
  ,boxDefaultColor = "rgb(210,214,220)"
  ,boxPrimaryColor = "rgba(44,222,235,1)"
  ,boxInfoColor = "rgb(210,214,220)"
  ,boxSuccessColor = "rgba(0,255,213,1)"
  ,boxWarningColor = "rgb(244,156,104)"
  ,boxDangerColor = "rgb(255,88,55)"
  
  ,tabBoxTabColor = "rgb(255,255,255)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(0,0,0)"
  ,tabBoxTabTextColorSelected = "rgb(0,0,0)"
  ,tabBoxBackColor = "rgb(255,255,255)"
  ,tabBoxHighlightColor = "rgba(44,222,235,1)"
  ,tabBoxBorderRadius = 5
  
  ### inputs
  ,buttonBackColor = "rgb(245,245,245)"
  ,buttonTextColor = "rgb(0,0,0)"
  ,buttonBorderColor = "rgb(200,200,200)"
  ,buttonBorderRadius = 5
  
  ,buttonBackColorHover = "rgb(235,235,235)"
  ,buttonTextColorHover = "rgb(100,100,100)"
  ,buttonBorderColorHover = "rgb(200,200,200)"
  
  ,textboxBackColor = "rgb(255,255,255)"
  ,textboxBorderColor = "rgb(200,200,200)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(245,245,245)"
  ,textboxBorderColorSelect = "rgb(200,200,200)"
  
  ### tables
  ,tableBackColor = "rgb(255,255,255)"
  ,tableBorderColor = "rgb(240,240,240)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
)

##### THEME END #####

###Function ###


ui <- dashboardPage(
  ##### LOGO START #####
  dashboardHeader(title = shinyDashboardLogoDIY(boldText = "Open Air",
                                                mainText = "Chicago",
                                                textSize = 16,
                                                badgeText = "BETA",
                                                badgeTextColor = "white",
                                                badgeTextSize = 2,
                                                badgeBackColor = chicago_red,
                                                badgeBorderRadius = 3)),
  ##### LOGO END #####
  
  dashboardSidebar(sidebarMenu(id = "sidebar",
                               menuItem("Home", tabName = "home", icon = icon("home")),
                               menuItem("About", tabName = "about", icon = icon("info")),
                               menuItem("PM2.5", tabName = "pm25", icon = icon("envira")))
                   ),
  
  dashboardBody(
    
    theme_air_chicago,
    
    tabItems(
      
      ##### HOME START #####
      tabItem(tabName = "home",
              fluidRow(
                box(width = 12,
                    h1("Home", align = "center")
                )),
              fluidRow(
                box(width = 4,
                    leafletOutput("homemap", height = mapheight),
                    checkboxGroupInput("homecheck", label = "", c("Show Mean" = "mean",
                                                                  "Rescale Data" = "rescale"),
                                       selected = c("mean"),
                                       width = '100%',
                                       inline = TRUE)),
                box(width = 8,
                    selectizeInput("homevar", "Select Variables for Comparison:",
                                     c("PM2.5" = "PM25")),
                                   options = list(maxItems = 7)),
                    plotlyOutput("homeplot", height = 445),
                    actionButton("clearshapes", "Clear Selection")))
      ),
      ##### HOME END #####
      ##### ABOUT START #####
      tabItem(tabName = "about",
              fluidRow(
                box(width = 12,
                    h1("About", align = "center"),
                    textOutput("abouttext")
                )),
              fluidRow(
                box(width = 6,
                    h1("Overview", align = "center", style = "color: #80ceff"),
                    p("Open Air Chicago is an interactive dashboard providing information on air quality for the greater Chicagoland area including Milwaukee. It includes direct measures of air quality as well as variables known to affect or relate to these variables. Each of the 16 examined variables has an individual page with a variable description, source information, and interactive visualization. Additionally, the “Home” tab offers the option to explore broader trends within the data for a single variable or among several variables both at the broader Chicagoland scale and the individual county level. All data used to generate the graphs and maps on the dashboard are available for access on the “Downloads” tab.")
                ),
                box(width = 6,
                    h1("Objectives", align = "center", style = "color: #c71414"),
                    p("The primary goal of this dashboard is to provide both researchers and the public at large with clean, free, and easily accessible data for all things air quality. While all data used in the dashboard is technically available for free online, the numerous formats, sources, and options provide for an unwelcoming landscape. By streamlining the process through which the data is accessed, the hope is to enable more people to spend more time actually analyzing the data and working to improve air quality. Additionally, Open Air Chicago hopes to do this by offering data visualization options to explore individual variable data as well as relationships between variables over time.")
                )),
              fluidRow(
                box(width = 6,
                    h1("Data", align = "center", style = "color: #c71414"),
                    p("All data used to create the dashboard is available online free of charge. Sources include the Environmental Protection Agency (EPA), National Aeronautics and Space Administration (NASA),  National Oceanic and Atmospheric Association (NOAA), United States Geological Survey (USGS), and OpenStreetMap. Specific sourcing information is available on individual dashboard pages. County-level aggregates for all variables are available for download at a monthly and quarterly temporal resolution as CSV files on the “Downloads” tab. Also available on the “Downloads” tab is a GeoTiff raster file containing all of the 1km resolution gridded data.")
                ),
                box(width = 6,
                    h1("Methodology", align = "center",  style = "color: #80ceff"),
                    p("In addition to differing in source and format, the raw data also exists at a variety of spatial and temporal resolutions. All data was aggregated to a standard, 1km resolution grid at both monthly and quarterly intervals. For the EPA sensor data, the gridded values were extracted from an Inverse Distance Weighted interpolation of sensor averages. Interpolated values for variables with fewer sensors will be less accurate than those with more sensors. Due to data availability, particularly with NASA’s remote-sensed Aerosol Optical Depth, individual variable pages provide visualizations of the quarterly aggregates to maximize coverage. For data not originally provided at a 1km resolution, unless otherwise noted on the “Source” tab on each variable page, the value assigned to each 1km cell is the mean of all measured values within it.")
                ))
      ),
      ##### ABOUT END #####
      tabItem(tabName = "pm25",
              fluidRow(
                box(width = 4,
                    tabsetPanel(
                      tabPanel(title = "Description",
                               h3(pm25.name),
                               p(pm25.source)),
                      tabPanel(title = "Source",
                               h4("Data Source"),
                               p(pm25.description))))
              ),
              box(width = 8,
                  sliderInput("Date", 
                              min = strptime("2020/12/01","%Y/%m/%d"), 
                              max = strptime("2021/01/24","%Y/%m/%d"),
                              value = strptime("2021/01/01","%Y/%m/%d"),
                              timeFormat = "%Y/%m/%d",
                              step = as.difftime(1, units = "days"),
                  animate = animationOptions(interval = 2000),
                  leafletOutput("pm25_map",height = mapheight)
              ))
      )
  )
)


mypalette <- colorBin( palette="YlOrBr", domain=quakes$mag, na.color="transparent")

server <- function(input, output) {
  
  
  ##### HOME START #####
  
 data_input<- reactive({county.pm25$`Daily Mean PM2.5 Concentration`})
 labels_input<- reactive({county.pm25$Date})

  output$homemap <- renderLeaflet({
    leaflet(counties) %>%
      addProviderTiles(provider = "OpenStreetMap.HOT") %>%
      setView(lat = "41.97736", lng = "-87.62255", zoom = 7) %>% 
      leaflet::addPolygons(weight = 1,
                           color = "gray",
                           layerId = counties$FIPS,
                           fillOpacity = 0.2,
                           label = counties$COUNTYNAME,
                           highlight = highlightOptions(
                             weight = 2,
                             color = "#666",
                             fillOpacity = 0.7,
                             bringToFront = TRUE))
  })
  
  ##### HOME END #####
  
  output$pm25_map <- renderLeaflet({
    leaflet(counties) %>%
      addProviderTiles(provider = "OpenStreetMap.HOT") %>%
      setView(lat = "41.97736", lng = "-87.62255", zoom = 7) %>% 
      addCircleMarkers(county.pm25$SITE_LONGITUDE,county.pm25$SITE_LATITUDE, 
                         fillColor = mypalette(data_input), fillOpacity = 0.7, color="white", radius=8, stroke=FALSE,
                         label = labels_input,
                         labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
        )

  })
}

shinyApp(ui, server) 

