# Chicago Open Air Quality Dashboard
The Chicago Open Air Quality Dashboard is an app for examining current and historical trends in many air quality indicators, centered around a 1km high-resolution particulate matter interpolation model. The app focuses on the 21-county Chicagoland area.

The web host for this app is available at https://herop.shinyapps.io/airq/

### Home
The home tab includes 2 explorer pages. 

On the `Region Explorer` page, the user can select a subset of counties (by clicking) and variables, allowing comparison of their trends between each other and to the mean trend.

On the `Health Explorer` page, two maps are provided, allowing the user to select types of weekly sensor data, and a choropleth underlay (either various historic data, or weekly COVID data). Individual sensors or zip codes can also be selected, to compare trends on the chart below.

The home tab also includes a link to the Healthy Regions and Policies Lab's Tree Equity Tool, and an about section.

### EPA Sensor Data
The EPA sensor data tab includes data for PM<sub>2.5</sub>, PM<sub>10</sub>, carbon monoxide, nitrogen dioxide, ozone, sulfur dioxide, and lead, from historic EPA sensor readings.

Data from individual sensors is provided, as well as an Inverse Distance Weighted (IDW) interpolation, and can be viewed at a monthly or quarterly resolution. Graphs are also provided showing the distributions of sensor readings in general, and compared to the current period.

### Meteorological Data
The meteorological data tab includes data for temperature, pressure, and precipitation, from historical FAA ASOS sensor readings.

Data from individual sensors is provided, as well as an IDW interpolation, and can be viewed at a monthly or quarterly resolution. Graphs are also provided showing the distributions of sensor readings in general, and compared to the current period.

### Remote-Sensed Data
The remote-sensed data tab includes a combination of satellite and surveyed data: Aerosol Optical Depth (AOD), normalized difference vegetation index (NDVI), Bidirectional Reflectance Factor (BRF), land cover, and elevation.

Data can be selected by quarter. The surfaces provided here are not interpolations.

### PM<sub>2.5</sub> Model
The PM<sub>2.5</sub> model tab gives the results of the lab's neural net PM<sub>2.5</sub> interpolation model.

Results are provided monthly, and present a better picture of the local trends in PM<sub>2.5</sub> than the corresponding IDW interpolation.

### Pollution Drivers
The pollution drivers tab provides data that estimate the density of pollutions sources in the Chicagoland area.

Density estimates for emissions sources, and road densities are provided.

### Chicago Health Atlas
Data for other health and population vulnerability information is provided by the Chicago Health Atlas: https://chicagohealthatlas.org

### Downloads
The data used in the map are made available here for download, including .csv data, .geoJSON spatial data, and .grd rasters.