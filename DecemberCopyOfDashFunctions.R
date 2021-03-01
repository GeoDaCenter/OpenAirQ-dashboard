

### Create Tab Page for Variables with Quarterly Data
generateQuarterlyTab <- function(tabname, variablename, variabledescription, sourcedescription,
                                  mapheight = 500) 
  {
  tabItem(tabName = tabname,
          fluidRow(
            box(width = 4,
              tabsetPanel(
                tabPanel(title = "Description",
                  h3(variablename),
                  p(variabledescription)),
                tabPanel(title = "Source",
                  h4("Data Source"),
                  p(sourcedescription))),
              radioGroupButtons(inputId = paste(tabname, "chi_zoom", sep = "_"),
                                              "Set View", 
                                              c("21 Counties" = "lac", 
                                                "Chicago" = "chi"))
              ),
            box(width = 8,
                sliderInput(paste(tabname, "dt", sep = "_"), "Select quarter:",
                            min = strptime("2014/01/01","%Y/%m/%d"), 
                            max = strptime("2018/12/31","%Y/%m/%d"),
                            value = strptime("2016/07/01","%Y/%m/%d"),
                            timeFormat = "%Y/%m",
                            step = as.difftime(92, units = "days"),
                            animate = animationOptions(interval = 5000)),
                leafletOutput(paste(tabname, "map", sep = "_"),height = mapheight),
                radioGroupButtons(paste(tabname, "rad", sep = "_"), "Select Color Palette", 
                             c("Overall" = "ovr", "Yearly" = "yr", "Quarterly" = "qtr"), 
                             selected = "ovr"))
              ))
}


### Create Tab Page for Variables with One Time Data
generateOneTimeTab <- function(tabname, variablename, variabledescription, sourcedescription,
                               mapheight = 500) {
  tabItem(tabName = tabname,
          fluidRow(
            box(width = 4,
              tabsetPanel(
                tabPanel(title = "Description",
                         h3(variablename),
                         p(variabledescription)),
                tabPanel(title = "Source",
                         h4("Data Source"),
                         p(sourcedescription))
                ),
              radioGroupButtons(inputId = paste(tabname, "chi_zoom", sep = "_"),
                                "Set View", 
                                c("21 Counties" = "lac", 
                                  "Chicago" = "chi"))
              ),
            box(width = 8,
                leafletOutput(paste(tabname, "map", sep = "_"), height = mapheight)
                
            )
              ))
}

##### Date Slider Output to Raster Layer Name
getLayerName <- function(in.date, variable, period = "qtr") {
  
  in.date <- as.Date(in.date)
  
  var.month <- month(in.date)
  var.yr <- year(in.date)
  
  var.qtr <- ceiling(var.month/3)
  
  var.yr <- substring(var.yr, 3, 4) 
  if(period == "qtr") {
    this.var.name <- paste(variable, var.qtr, var.yr, sep = "_")
  }
  else if (period == "mon") {
    this.var.name <- paste(variable, var.month, var.yr, sep = "_")
  }
}

##### Generate Leaflet Map 
dashMap <- function(layername, layerpal, raster, area, layerId, rasterOpacity = 0.8,
                    EPApoints = NULL, VarName = NULL) {
  
    dMap <- leaflet(layername) %>%
    addProviderTiles("OpenStreetMap.HOT") %>%
    addRasterImage(raster[[layername]], opacity = rasterOpacity, colors = layerpal) %>%
    leaflet::addLegend(pal = layerpal, values = values(raster[[layername]]), title = gsub("_.*","",layername)) %>%
    addPolygons(data = area, 
                color = "darkslategray",
                fillOpacity  = 0.00, 
                stroke = TRUE,
                opacity = 1,
                layerId = layerId,
                weight = 1,
                highlight = highlightOptions(
                  weight = 2, 
                  color = "gray", 
                  fillOpacity = 0.05))
    
    if (!is.null(EPApoints)) {
      dMap <- dMap %>%
        addCircles(lng = EPApoints$Longitude[EPApoints$Var == VarName],
                 lat = EPApoints$Latitude[EPApoints$Var == VarName],
                 radius = 2, color = "black", opacity = 0.9)
    }
    
    dMap
}

##### For use in observe function with slider

sliderProxy <- function(mapname, layername, layerpal, raster, rasterOpacity = 0.8) {
  leafletProxy(mapname) %>%
    clearControls() %>%
    clearImages() %>%
    addRasterImage(raster[[layername]], opacity = rasterOpacity, colors = layerpal) %>%
    leaflet::addLegend(pal = layerpal, values = values(raster[[layername]]), title = gsub("_.*","",layername))
}


##### Zoom on click 
# Proxy: Leaflet map name
# Click: Click input
# Area: Polygons
zoomMap <- function(proxy, click, area) {
  this.proxy <- leafletProxy(proxy)
  fips <- click$id
  county <- area$COUNTYNAME[which(area$FIPS == fips)]
  
  if(!is.null(fips)) {
  if(fips != "Highlighted") {
    this.proxy %>% 
      flyTo(lng = click$lng, lat = click$lat, zoom = 8) %>%
      addPolygons(data=area[which(area$FIPS == fips),][1],
                  color = "grey", layerId = "Highlighted",
                  opacity = 0.01,
                  label = paste(county, " County"),
                  labelOptions = labelOptions(noHide = T)
                  )}
  else if (fips == "Highlighted") {
    this.proxy %>%
      removeShape(layerId = "Highlighted") %>%
      flyTo(lng = area$LON[12],
              lat = area$LAT[12],
              zoom = 7)
  }
  }
}



zoomChiMap <- function(proxy, click, area) {
  
  this.proxy <- leafletProxy(proxy)
  ca.num <- click$id
  
  ca <- area$community[which(area$area_numbe == ca.num)]
  
  if(!is.null(ca.num)) {
  if(ca.num != "Highlighted") {
    
    if(!is.na(as.numeric(ca.num)) && as.numeric(ca.num) > 77) { #prevent crash when county clicked
      this.proxy %>%
        flyTo(lng = -87.660456,
              lat = 41.845027,
              zoom = 10)
    } else {
      this.proxy %>% 
        flyTo(lng = click$lng, lat = click$lat, zoom = 12) %>% #change to setView if too slow
        addPolygons(data=area[which(area$area_numbe == ca.num),][1],
                    color = "grey", layerId = "Highlighted",
                    opacity = 0.01,
                    label = area$community[which(area$area_numbe == ca.num)],
                    labelOptions = labelOptions(noHide = T)
        )
    }
  }
  else if (ca.num == "Highlighted") {
    this.proxy %>%
      removeShape(layerId = "Highlighted") %>%
      flyTo(lng = -87.660456,
            lat = 41.845027,
            zoom = 10)
  }
  }
}

chiView <- function(proxy, area, EPApoints = NULL, VarName = NULL) {
  
  this.proxy <- leafletProxy(proxy)
  this.proxy <- this.proxy %>%
    clearShapes()
  
  this.proxy <- this.proxy %>%
    flyTo(lng = -87.660456,
          lat = 41.845027,
          zoom = 10) %>%
    addPolygons(data = area,
                color = "darkslategray",
                stroke = T,
                opacity = 1,
                layerId = area$area_numbe,
                weight = 1,
                fillOpacity = 0.01)
  
  if (!is.null(EPApoints)) {
    this.proxy <- this.proxy %>%
      addCircles(lng = EPApoints$Longitude[EPApoints$Var == VarName],
                 lat = EPApoints$Latitude[EPApoints$Var == VarName],
                 radius = 2, color = "black", opacity = 0.9)
  }
  
  this.proxy
}

lacView <- function(proxy, area, EPApoints = NULL, VarName = Null) {
  
  this.proxy <- leafletProxy(proxy)
  this.proxy <- this.proxy %>%
    clearShapes() 
    
  this.proxy <- this.proxy %>%
    flyTo(lat = "41.97736", lng = "-87.62255", zoom = 7) %>% 
    addPolygons(data = area, 
                color = "darkslategray",
                fillOpacity  = 0.01, 
                stroke = TRUE,
                opacity = 1,
                layerId = area$FIPS,
                weight = 1,
                highlight = highlightOptions(
                  weight = 2, 
                  color = "gray", 
                  fillOpacity = 0.05))
  
  if (!is.null(EPApoints)) {
    this.proxy <- this.proxy %>%
      addCircles(lng = EPApoints$Longitude[EPApoints$Var == VarName],
                 lat = EPApoints$Latitude[EPApoints$Var == VarName],
                 radius = 2, color = "black", opacity = 0.9)
  }
  
  this.proxy
  
}





#Creates palette
#qtr: Palette covers single quarter
#ovr: Palette covers entire 5 year period (input "AOD", "NDVI", etc)

palFromLayer <- function(layername, style = "ovr", colors = c("green", "yellow", "orange", "red"), 
                         raster, nacolor = "transparent") {
  if(style == "qtr" || style == "mon") {
    
    this.raster <- raster[[layername]]
    
    max <- raster::maxValue(this.raster)
    min <- raster::minValue(this.raster)
    
  } else if (style == "ovr") {
    layer.var <- substring(layername, 1, 3)
    
    this.raster <- raster[[which(grepl(layer.var, names(raster)))]]
    
    max <- max(maxValue(this.raster))
    min <- min(minValue(this.raster))
    
  } else if (style == "yr") {
    layer.var <- substring(layername, 1, 3)
    var.raster <- raster[[which(grepl(layer.var, names(raster)))]]

    layer.yr <- substring(layername, (nchar(layername) - 1), nchar(layername))
    this.raster <- var.raster[[which(grepl(layer.yr, names(var.raster)))]]

    max <- max(maxValue(this.raster))
    min <- min(minValue(this.raster))
  }

  inc <- (max - min) / 10 
    
  breaks <- seq(from = min, to = max, inc)
    
  pal <- colorNumeric(colors, breaks, na.color = nacolor) 

}





