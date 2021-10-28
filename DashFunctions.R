### Create Tab Page for Variables with Quarterly Data
generateDynaTab <- function(tabname, variablename, variabledescription, sourcedescription,
                                 radselect = "ovr", mapheight = "90vh") 
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
                fluidRow(
                  column(width = 5,
                         radioGroupButtons(inputId = paste(tabname, "chi_zoom", sep = "_"),
                                           "Set View", 
                                           c("21 Counties" = "lac", 
                                             "Chicago" = "chi"))),
                  column(width = 7,
                         radioGroupButtons(paste(tabname, "rad", sep = "_"), "Select Color Palette", 
                                           c("Overall" = "ovr", "Yearly" = "yr", "Monthly" = "mon"), 
                                           selected = radselect)),
                  column(width = 2,
                         radioButtons(paste(tabname, "res", sep = "_"), "Timestep:",
                                      choices = c("Monthly" = "mon",
                                                  "Quarterly" = "qtr"),
                                      selected = "mon")),
                  column(width = 10,
                         sliderTextInput(paste(tabname, "dt", sep = "_"), "Select Month:",
                                         choices = format(seq.Date(as.Date("2014/01/01"), as.Date("2018/12/01"), by="month"),
                                                          "%Y/%m"),
                                         selected = "2016/07", grid = TRUE))),
                tabsetPanel(
                  tabPanel(title = "Distribution",
                           plotOutput(paste(tabname, "density", sep = "_"))),
                  tabPanel(title = "Time Series",
                           plotOutput(paste(tabname, "time", sep = "_"))),
                  tabPanel(title = "Summary Table",
                           uiOutput(paste(tabname, "sum", sep = "_"))))),
            box(width = 8,
                leafletOutput(paste(tabname, "map", sep = "_"),height = mapheight),
                )
          ))
}

### Create Tab Page for Variables with Quarterly Data
generateQuarterlyTab <- function(tabname, variablename, variabledescription, sourcedescription,
                                  mapheight = "90vh") 
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
              column(width = 5,
                radioGroupButtons(inputId = paste(tabname, "chi_zoom", sep = "_"),
                                              "Set View", 
                                              c("21 Counties" = "lac", 
                                                "Chicago" = "chi"))),
              column(width = 7,
                radioGroupButtons(paste(tabname, "rad", sep = "_"), "Select Color Palette", 
                                c("Overall" = "ovr", "Yearly" = "yr", "Quarterly" = "qtr"), 
                                selected = "ovr")),
              sliderInput(paste(tabname, "dt", sep = "_"), "Select quarter:",
                          min = strptime("2014/01/01","%Y/%m/%d"), 
                          max = strptime("2018/12/31","%Y/%m/%d"),
                          value = strptime("2016/07/01","%Y/%m/%d"),
                          timeFormat = "%Y/%m",
                          step = as.difftime(92, units = "days"),
                          animate = animationOptions(interval = 5000))
              ),
            box(width = 8,
                leafletOutput(paste(tabname, "map", sep = "_"),height = mapheight)
               )
              ))
}


### Create Tab Page for Variables with One Time Data
generateOneTimeTab <- function(tabname, variablename, variabledescription, sourcedescription,
                               mapviewselected = "lac", mapheight = "70vh") {
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
                                  "Chicago" = "chi"),
                                selected = mapviewselected)
              ),
            box(width = 8,
                leafletOutput(paste(tabname, "map", sep = "_"), height = mapheight)
                
            )
              ))
}

##### Date Slider Output to Raster Layer Name
getLayerName <- function(in.date, variable, period = "qtr") {
  
  in.date <- as.Date(paste0(in.date, "/01"))
  
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
                    EPApoints = NULL, VarName = NULL, units = "") {
  
    dMap <- leaflet(layername) %>%
    addProviderTiles("OpenStreetMap.HOT") %>%
    addRasterImage(raster[[layername]], opacity = rasterOpacity, colors = layerpal) %>%
    leaflet::addLegend(pal = layerpal, values = values(raster[[layername]]), title = paste(gsub("_.*","",layername), units)) %>%
    addMapPane("polys", zIndex=410) %>%
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
                  fillOpacity = 0.05),
                options = pathOptions(pane = "polys"))
    
    if (!is.null(EPApoints)) {
      points <- na.omit(EPApoints[layername])
      longlat <- st_coordinates(points)
      points <- cbind(longlat, st_drop_geometry(points))
      dMap <- dMap %>%
              addMapPane("points", zIndex = 420) %>%
              addCircleMarkers(lng = points$X,
                               lat = points$Y,
                               fillColor = layerpal(points[,3]),
                               fillOpacity = 1,
                               stroke = TRUE,
                               color = 'black',
                               dashArray = "4 1 2 3",
                               opacity = 0.6,
                               radius = 5,
                               weight = 2,
                               label = points[, 3],
                               options = pathOptions(pane = "points"))
    }
    
    dMap
}

##### For use in observe function with slider

sliderProxy <- function(mapname, layername, layerpal, raster, rasterOpacity = 0.8, units = "", EPApoints = NULL) {
  leafletProxy(mapname) %>%
    clearControls() %>%
    clearImages() %>%
    clearMarkers() %>%
    addRasterImage(raster[[layername]], opacity = rasterOpacity, colors = layerpal) %>%
    leaflet::addLegend(pal = layerpal, values = values(raster[[layername]]), title = paste(gsub("_.*","",layername), units))
  
  if (!is.null(EPApoints)) {
    points <- na.omit(EPApoints[layername])
    longlat <- st_coordinates(points)
    points <- cbind(longlat, st_drop_geometry(points))
    leafletProxy(mapname) %>%
      addCircleMarkers(lng = points$X,
                       lat = points$Y,
                       fillColor = layerpal(points[,3]),
                       fillOpacity = 1,
                       stroke = TRUE,
                       color = 'black',
                       dashArray = "4 1 2 3",
                       opacity = 0.6,
                       radius = 5,
                       weight = 2,
                       label = points[, 3],
                       options = pathOptions(pane = "points")) %>%
    addLegend(title = "Icon Key",
              position = "bottomright",
              colors = c("white; width:15px; height:15px; border:2px solid black; border-radius: 50%",
                         "yellow; width:25px; height:25px; border:3px solid yellow; border-radius: 0%"),
              labels = c("<div style='display: inline-block;height:15px;margin-top: 4px;line-height:15px;'> Observed Reading</div>",
                         "<div style='display: inline-block;height:25px;margin-top: 4px;line-height:25px;'> Interpolated Estimate </div>"))
  }
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
                fillOpacity = 0.01,
                options = list(zindex = 1))

  
  this.proxy
}

lacView <- function(proxy, area, EPApoints = NULL, VarName = NULL) {
  
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
  this.proxy
  
}





#Creates palette
#qtr: Palette covers single quarter
#ovr: Palette covers entire 5 year period (input "AOD", "NDVI", etc)
#pt.bounds: Extends palette to capture sensor readings above or below raster bounds

palFromLayer <- function(layername, style = "ovr", colors = c("green", "yellow", "orange", "red"), 
                         raster, nacolor = "transparent", pt.bounds = NULL) {
  if(style == "qtr" || style == "mon") {
    
    this.raster <- raster[[layername]]
    
    max <- raster::maxValue(this.raster)
    min <- raster::minValue(this.raster)
    
  } else if (style == "ovr") {
    layer.var <- substring(layername, 1, 3)
    
    #### Hacky fix until "Pressure" and "Precip" are named differently.
    if (layer.var == "Pre"){ layer.var <- substring(layername, 1, 4) }
    
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
  
  if (!is.null(pt.bounds))
    min = min(min, pt.bounds[1])
    max = max(max, pt.bounds[2])

  inc <- (max - min) / 10 
    
  breaks <- seq(from = min, to = max, inc)
    
  pal <- colorNumeric(colors, breaks, na.color = nacolor) 

}

#Create palettes from PM2.5 layer --> easy to modify to make layername an argument
palFromVectorLayer <- function(filtereddata, completedata, style = "ovr", 
                               colors = c("green", "yellow", "orange", "red"), 
                               nacolor = "transparent") {
  if(style == "qtr" || style == "mon") {
    
    max <- max(filtereddata$avg_pm25)
    min <- min(filtereddata$avg_pm25)
    
  } else if (style == "ovr") {
    
    max <- max(completedata$avg_pm25)
    min <- min(completedata$avg_pm25)
    
  } else if (style == "yr") {
    
    this.yr <- substring(stringr::str_extract(filtereddata$moyr[1], "-(.*)"), first = 2, last = 5)
    
    
    yr.data <- completedata[grep(this.yr, completedata$moyr),]
    
    max <- max(yr.data$avg_pm25)
    min <- min(yr.data$avg_pm25)
  }

  inc <- (max - min) / 10 
  
  breaks <- seq(from = min, to = max, inc)

  pal <- colorNumeric(colors, breaks, na.color = nacolor) 

}



### Converts column to HTML labels; maps NA values to their date of last update
getLabels <- function(date, dataframe, varname) {
  col.format <- paste(varname, "%Y%m%d", sep = "_")
  idx <- which(colnames(dataframe) == format(date, col.format))
  col <- dataframe[, idx]
  labels <- sprintf(as.character(round(col, 3)))
  na.idx <- which(is.na(col))
  search <- dataframe[na.idx, idx:ncol(dataframe)] # slice of dataframe to search for last update
  # week is shifted back here, to be consistent with the dataproc
  # note: max.col doesn't work if there is no last update available; this does not happen currently
  update <- strptime(names(search)[max.col(!is.na(search), "first")], col.format) - days(6) 
  update[update == date - days(6)] <- NA # if update == today, no last update exists
  repl <- paste("NA<br>Last Updated:", # Create HTML strings for last updates
                update, 
                sep = " ") 
  labels[na.idx] <- repl
  labels <- paste(dataframe$name, "<br>", labels, sep = "")
  labels %>% lapply(htmltools::HTML)
}


switchTimeRes <- function(res){
  
  new_vals = list()
  
  if (res == "mon"){
    new_vals[[1]] = "Select Month:"
    new_vals[[2]] = format(seq.Date(as.Date("2014/01/01"), as.Date("2018/12/01"), by="month"),
                          "%Y/%m")
    new_vals[[3]] = c("Overall" = "ovr", "Yearly" = "yr", "Monthly" = "mon")
  }
  else if (res == "qtr"){
    new_vals[[1]] = "Select Quarter:"
    new_vals[[2]] = format(seq.Date(as.Date("2014/01/01"), as.Date("2018/12/01"), by="quarter"),
                          "%Y/%m")
    new_vals[[3]] = c("Overall" = "ovr", "Yearly" = "yr", "Quarterly" = "qtr")
  }
  
  return(new_vals)
}

densityPlotLabels <- function(date, res){
  date = paste0(date,"/01")
  labels = vector()
  if (res == "mon"){
    labels[1] = "All Other Months"
    labels[2] = format.Date(date, "%B %Y")
  }
  else {
    labels[1] = "All Other Quarters"
    labels[2] = paste0("Q", quarter(date), " ", year(date))
  }
  
  return(labels)
}


getPointData <- function(res){
  if (res == "mon"){
    points = epa.monthly
  }
  else{
    points = epa.quarterly
  }
  return(points)
}

getRasterData <- function(res){
  if (res == "mon"){
    raster = monthly.raster
  }
  else{
    raster = master.raster
  }
  
  return(raster)
}


getPointFAA <- function(res){
  if (res == "mon"){
    points = faa.monthly
  }
  else{
    points = faa.quarterly
  }
  return(points)
}

getRasterFAA <- function(res){
  if (res == "mon"){
    raster = faa.mon.raster
    ### Super hacky thing to fix monthly FAA names to match master.raster.
    n = nchar(names(raster))
    names(raster) = stri_sub_replace(names(raster), from = (n-3), to = (n-2), value="")
  }
  else{
    raster = master.raster
  }
  
  return(raster)
}

density_plot <- function(date, varname, res, points, xlab){
  
  this.name <- getLayerName(date, varname, period = res)
  labels <- densityPlotLabels(date, res)
  
  full_points <- gather(st_drop_geometry(points[which(grepl(varname, names(points)))]))
  print(this.name)
  
  ggplot(full_points) + geom_density(aes(value, fill = (key == this.name)), alpha = 0.6) + 
    theme_tufte() + 
    theme(legend.position = c(.87, .87)) + 
    labs(y = "Density", x = paste0(varname, " (", xlab, ")"), fill = "") + 
    scale_fill_manual(labels = labels, values = c("grey", "red")) 
}



boxplot_dist <- function(date, varname, res, points, ylab){
  
  this.name <- getLayerName(date, varname, period = res)
  labels <- densityPlotLabels(date, res)
  
  full_points <- gather(st_drop_geometry(points[which(grepl(varname, names(points)))]))

  
  ggplot(full_points) + geom_boxplot(aes(y = value, x = (key == this.name), fill = (key == this.name)), alpha = 0.6) + 
    theme_tufte() + 
    theme(legend.position = "none") + 
    labs(y = paste0(varname, " (", ylab, ")"), x = "", fill = "") +
    scale_x_discrete(labels = labels) + 
    scale_fill_manual(labels = labels, values = c("grey", "red"))
}