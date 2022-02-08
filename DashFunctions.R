# Legend Reversing function
# Taken from https://github.com/rstudio/leaflet/issues/256#issuecomment-440290201
# Addresses Leaflet issue of presenting legend in unintuitive direction
addLegendDecreasing <- function (map, position = c("topright", "bottomright", "bottomleft","topleft"),
                                  pal, values, na.label = "NA", bins = 7, colors, 
                                  opacity = 0.5, labels = NULL, labFormat = labelFormat(), 
                                  title = NULL, className = "info legend", layerId = NULL, 
                                  group = NULL, data = getMapData(map), decreasing = FALSE) {
  
  position <- match.arg(position)
  type <- "unknown"
  na.color <- NULL
  extra <- NULL
  if (!missing(pal)) {
    if (!missing(colors)) 
      stop("You must provide either 'pal' or 'colors' (not both)")
    if (missing(title) && inherits(values, "formula")) 
      title <- deparse(values[[2]])
    values <- evalFormula(values, data)
    type <- attr(pal, "colorType", exact = TRUE)
    args <- attr(pal, "colorArgs", exact = TRUE)
    na.color <- args$na.color
    if (!is.null(na.color) && col2rgb(na.color, alpha = TRUE)[[4]] == 
        0) {
      na.color <- NULL
    }
    if (type != "numeric" && !missing(bins)) 
      warning("'bins' is ignored because the palette type is not numeric")
    if (type == "numeric") {
      cuts <- if (length(bins) == 1) 
        pretty(values, bins)
      else bins   
      if (length(bins) > 2) 
        if (!all(abs(diff(bins, differences = 2)) <= 
                 sqrt(.Machine$double.eps))) 
          stop("The vector of breaks 'bins' must be equally spaced")
      n <- length(cuts)
      r <- range(values, na.rm = TRUE)
      cuts <- cuts[cuts >= r[1] & cuts <= r[2]]
      n <- length(cuts)
      p <- (cuts - r[1])/(r[2] - r[1])
      extra <- list(p_1 = p[1], p_n = p[n])
      p <- c("", paste0(100 * p, "%"), "")
      if (decreasing == TRUE){
        colors <- pal(rev(c(r[1], cuts, r[2])))
        labels <- rev(labFormat(type = "numeric", cuts))
      }else{
        colors <- pal(c(r[1], cuts, r[2]))
        labels <- rev(labFormat(type = "numeric", cuts))
      }
      colors <- paste(colors, p, sep = " ", collapse = ", ")
    }
    else if (type == "bin") {
      cuts <- args$bins
      n <- length(cuts)
      mids <- (cuts[-1] + cuts[-n])/2
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "bin", cuts))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "bin", cuts)
      }
    }
    else if (type == "quantile") {
      p <- args$probs
      n <- length(p)
      cuts <- quantile(values, probs = p, na.rm = TRUE)
      mids <- quantile(values, probs = (p[-1] + p[-n])/2, na.rm = TRUE)
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "quantile", cuts, p))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "quantile", cuts, p)
      }
    }
    else if (type == "factor") {
      v <- sort(unique(na.omit(values)))
      colors <- pal(v)
      labels <- labFormat(type = "factor", v)
      if (decreasing == TRUE){
        colors <- pal(rev(v))
        labels <- rev(labFormat(type = "factor", v))
      }else{
        colors <- pal(v)
        labels <- labFormat(type = "factor", v)
      }
    }
    else stop("Palette function not supported")
    if (!any(is.na(values))) 
      na.color <- NULL
  }
  else {
    if (length(colors) != length(labels)) 
      stop("'colors' and 'labels' must be of the same length")
  }
  legend <- list(colors = I(unname(colors)), labels = I(unname(labels)), 
                 na_color = na.color, na_label = na.label, opacity = opacity, 
                 position = position, type = type, title = title, extra = extra, 
                 layerId = layerId, className = className, group = group)
  invokeMethod(map, data, "addLegend", legend)
}






#' Generate tab for historical data with quarterly and monthly aggregations
#' 
#' @param tabname a string for output
#' @param variablename a string for dataset filtering
#' @param variabledescription a string for output
#' @param sourcedescription a string for output
#' @param radselect a string designating the palette scope
#' @param mapheight a string for map size
#' 
generateDynaTab <- function(tabname, variablename, variabledescription, sourcedescription, mapheight = "90vh") 
{
  tabItem(tabName = tabname,
          use_prompt(),
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
                         add_prompt(
                           radioGroupButtons(inputId = paste(tabname, "chi_zoom", sep = "_"),
                                             "Set View", 
                                             c("21 Counties" = "lac", 
                                               "Chicago" = "chi")), 
                           position = "bottom", message = "Zoom in or out of map",
                           type = "info", animate = TRUE
                         )),
                  column(width = 7,
                         add_prompt(
                           radioGroupButtons(paste(tabname, "rad", sep = "_"), "Select Color Scale", 
                                             c("Overall" = "ovr", "Yearly" = "yr", "Monthly" = "mon"), 
                                             selected = "mon"),
                         position = "bottom", message = "Select if map color should be \n relative to all, year's, or selected readings",
                         type = "info", animate = TRUE
                         )),
                  column(width = 2,
                         radioButtons(paste(tabname, "res", sep = "_"), "Timestep:",
                                      choices = c("Monthly" = "mon",
                                                  "Quarterly" = "qtr"),
                                      selected = "mon")),
                  column(width = 10,
                         sliderTextInput(paste(tabname, "dt", sep = "_"), "Select Month:",
                                         choices = format(seq.Date(as.Date("2014/01/01"), as.Date("2018/12/01"), by="month"),
                                                          "%Y/%m"),
                                         selected = "2016/07", grid = FALSE))),
                  plotOutput(paste(tabname, "time", sep = "_")),
                  plotOutput(paste(tabname, "density", sep = "_"))),
            box(width = 8,
                leafletOutput(paste(tabname, "map", sep = "_"),height = mapheight),
                )
          ))
}

#' Generate tab for historical data with only quarterly aggregations
#' 
#' @param tabname a string for output
#' @param variablename a string for dataset filtering
#' @param variabledescription a string for output
#' @param sourcedescription a string for output
#' @param mapheight a string for map size
#' 
generateQuarterlyTab <- function(tabname, variablename, variabledescription, sourcedescription,
                                  mapheight = "90vh") 
  {
  tabItem(tabName = tabname,
          use_prompt(),
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
                     add_prompt(
                       radioGroupButtons(inputId = paste(tabname, "chi_zoom", sep = "_"),
                                         "Set View", 
                                         c("21 Counties" = "lac", 
                                           "Chicago" = "chi")), 
                       position = "bottom", message = "Zoom in or out of map",
                       type = "info", animate = TRUE
                     )),
              column(width = 7,
                     add_prompt(
                       radioGroupButtons(paste(tabname, "rad", sep = "_"), "Select Color Palette", 
                                         c("Overall" = "ovr", "Yearly" = "yr", "Quarterly" = "qtr"), 
                                         selected = "ovr"),
                       position = "bottom", message = "Select if map color should be \n relative to all, year's, or selected readings",
                       type = "info", animate = TRUE
                     )
                ),
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


#' Generate tab for historical data with constant data
#' 
#' @param tabname a string for output
#' @param variablename a string for dataset filtering
#' @param variabledescription a string for output
#' @param sourcedescription a string for output
#' @param mapviewselected a string for initial map zoom
#' @param mapheight a string for map size
#' 
generateOneTimeTab <- function(tabname, variablename, variabledescription, sourcedescription,
                               mapviewselected = "lac", mapheight = "90vh") {
  tabItem(tabName = tabname,
          use_prompt(),
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
              add_prompt(
                radioGroupButtons(inputId = paste(tabname, "chi_zoom", sep = "_"),
                                  "Set View", 
                                  c("21 Counties" = "lac", 
                                    "Chicago" = "chi")), 
                position = "bottom", message = "Zoom in or out of map",
                type = "info", animate = TRUE
              )),
            box(width = 8,
                leafletOutput(paste(tabname, "map", sep = "_"), height = mapheight)
            )
              ))
}

#' Reformats user input from slider for dataset filtering
#' 
#' @param in.date the user selected date
#' @param variable a string for dataset filtering
#' @param period the user selected timestep
#' 
#' @returns a formatted string to match dataset conventions
#' 
#' @examples 
#' getLayerName("2017-02-01", "PM10", "qtr") returns "PM10_1_17"
#' getLayerName("2014-11-01", "Temp", "mon") returns "Temp_11_14" 
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
  return(this.var.name)
}


#' Creates a raster for the given variable at the selected timestep
#' Also creates a point layer for observations, if available
#' 
#' @param layername a formatted string of the selected date and variable
#' @param layerpal a color palette covering the raster's range
#' @param raster a set of rasters
#' @param area a map
#' @param layerId a mapping variable
#' @param rasterOpacity a number
#' @param EPApoints a set of point observations
#' @param units a string for the units of the raster/point values
#' 
#' @returns a leaflet map with a raster layer (and point layer if provided)
dashMap <- function(layername, layerpal, raster, area, layerId, rasterOpacity = 0.8,
                    EPApoints = NULL, units = "") {
  
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
    
    return(dMap)
}


#' Clears the existing map layers and adds new layers based on change
#' in user selected timestep
#' 
#' @param mapname the leaflet map to update
#' @param layername a formatted string of the selected date and variable
#' @param layerpal a color palette covering the raster's range
#' @param raster a set of rasters
#' @param area a map
#' @param layerId a mapping variable
#' @param rasterOpacity a number
#' @param EPApoints a set of point observations
#' @param units a string for the units of the raster/point values
sliderProxy <- function(mapname, layername, layerpal, raster, rasterOpacity = 0.8, units = "", EPApoints = NULL) {
  leafletProxy(mapname) %>%
    clearControls() %>%
    clearImages() %>%
    clearMarkers() %>%
    addRasterImage(raster[[layername]], opacity = rasterOpacity, colors = layerpal) %>%
    addLegendDecreasing(pal = layerpal, values = values(raster[[layername]]), title = paste(gsub("_.*","",layername), units), decreasing = TRUE)
  
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


#' Updates leaflet map at county level based on user click input
#' 
#' @param proxy leaflet map
#' @param click user click input
#' @param area polygon layer for map
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


#' Updates leaflet map at city level based on user click input
#' 
#' @param proxy leaflet map
#' @param click user click input
#' @param area polygon layer for map
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

#' Zooms leaflet map to city
#' 
#' @param proxy leaflet map
#' @param area polygon layer for city
chiView <- function(proxy, area) {
  
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

#' Zooms leaflet map to counties
#' 
#' @param proxy leaflet map
#' @param area polygon layer for counties
lacView <- function(proxy, area) {
  
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

#' Creates a color palette for mapping based on select timeframe
#' 
#' @param layername a formatted string of the selected date and variable
#' @param style the timeframe of values to consider
#'  'ovr' = all values, 'yr' = selected calendar year, 'qtr'/'mon' = selected quarter/month
#' @param colors general colors to range
#' @param raster set of rasters
#' @param nacolor color value for missing values
#' @param pt.bounds extensions to domain of color bounds
#' 
#' @returns a color palette for mapping
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
  
  return(pal)
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



#' Converts column values to HTML labels. Maps NAs to last updated value
#' 
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


#' Switches slider timesteps between monthly/quarterly options
#' 
#' @param res the user-selected timestep length
#' 
#' @returns a list of quarter or month separated dates and a new palette button title
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

#' Formats selected date and legend for density plot
#' 
#' @param date the user-selected date
#' @param res the user-selected timestep length
#' 
#' @returns a string for the legend, and a formatted date for the plot
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

#' Creates a density plot comparing the user-selected date observations to all other observations
#' 
#' @param date the user-selected date
#' @param varname the tab variable
#' @param res the user-selected timestep length
#' @param points a dataset of observations
#' @param xlab a formatted plot label
#' 
#' @returns a density plot
density_plot <- function(date, varname, res, points, xlab){
  
  this.name <- getLayerName(date, varname, period = res)
  labels <- densityPlotLabels(date, res)
  
  full_points <- gather(st_drop_geometry(points[which(grepl(varname, names(points)))]))
  
  dens <- ggplot(full_points) + geom_density(aes(value, fill = (key == this.name)), alpha = 0.6) + 
    theme_tufte(base_size = 15) + 
    theme(legend.position = c(.87, .87)) + 
    labs(y = "Density", x = paste0(varname, " (", xlab, ")"), fill = "", title = "Distribution Comparison") + 
    scale_fill_manual(labels = labels, values = c("grey", "red")) 
  
  return(dens)
}


#' Creates a boxplot comparing the user-selected date observations to all other observations
#' 
#' @param date the user-selected date
#' @param varname the tab variable
#' @param res the user-selected timestep length
#' @param points a dataset of observations
#' @param ylab a formatted plot label
#' 
#' @returns a boxplot
boxplot_dist <- function(date, varname, res, points, ylab){
  
  this.name <- getLayerName(date, varname, period = res)
  labels <- densityPlotLabels(date, res)
  
  full_points <- gather(st_drop_geometry(points[which(grepl(varname, names(points)))]))

  
  box <- ggplot(full_points) + geom_boxplot(aes(y = value, x = (key == this.name), fill = (key == this.name)), alpha = 0.6) + 
    theme_tufte(base_size = 15) + 
    theme(legend.position = "none") + 
    labs(y = paste0(varname, " (", ylab, ")"), x = "", fill = "", title = "Distribution Comparison") +
    scale_x_discrete(labels = labels) + 
    scale_fill_manual(labels = labels, values = c("grey", "red"))
  
  return(box)
}

#' Creates a time series plot of the full set of observations. Draws a line at 
#' the current user-selected month/quarter
#' 
#' @param in.date the user-selected date
#' @param varname the tab variable
#' @param res the user-selected timestep length
#' @param points a dataset of observations
#' @param ylab a formatted date for the plot labels
#' 
#' @returns a time series plot
time_plot <- function(in.date, varname, res, points, ylab){
  
  full_points <- st_drop_geometry(points[which(grepl(varname, names(points)))])
  full_points = cbind(full_points, "Location" = 1:nrow(full_points))
  
  full_points = na.omit(gather(full_points, "date", "value", -Location))
  
  
  full_points$date = str_remove(full_points$date, paste0(varname, "_"))
  
  if (res == "mon"){
    full_points$date = as.yearmon(full_points$date, "%m_%y")
    in.date = as.yearmon(in.date, "%Y/%m")
  }
  else {
    full_points$date = as.yearqtr(full_points$date, "%q_%y")
    in.date = as.yearqtr(in.date, "%Y/%m")
  }
  
  tplot <- ggplot(full_points) + geom_line(aes(x = date, y = value, group = Location), alpha = 0.1) +
    theme_tufte(base_size = 15) + 
    labs(y = ylab, x = "Date", title = "Sensor Readings Over Time") + 
    geom_vline(xintercept = in.date, color = "red")
  
  return(tplot)
}

#' Stores selected palettes for all leaflet maps
#' 
#' @param varName the variable to pull a palette for
#' 
#' @returns a color scheme
get_colors <- function(varName){
  if (varName == "pm25"){
    return(inferno(100, alpha = 0.8, direction = -1))
  }
  else if(varName =="pm10"){
    return(inferno(100, alpha = 0.8, direction = -1))
  }
  else if(varName =="co"){
    return(cividis(100, alpha = 0.8, direction = -1))
  }
  else if(varName =="no2"){
    return(cividis(100, alpha = 0.8, direction = -1))
  }
  else if(varName =="o3"){
    return(magma(100, alpha = 0.8, direction = -1))
  }
  else if(varName =="so2"){
    return(magma(100, alpha = 0.8, direction = -1))
  }
  else if(varName =="pb"){
    return(cividis(100, alpha = 0.8, begin = 0.5, direction = -1))
  }
  else if(varName =="pe"){
    return(inferno(100, alpha = 1, begin = 0.5, end = 1, direction = -1))
  }
  else if(varName == "temp"){
    return(viridis(100, alpha = 0.8))
  }
  else if(varName == "pressure"){
    return(cividis(100, begin = 0, alpha = 0.8))
  }
  else if(varName == "precip"){
    return(viridis(100, alpha = 0.8))
  }
  else if(varName == "elevation"){
    return(viridis(100, alpha = 0.8))
  }
  
}
