
### Directories

CDPH_Permits - point emissions sources; [here](https://geodacenter.github.io/OpenAirQ-toolkit/04-ToolkitPointsToSurfaces.html) is a link to a toolkit chapter that goes over how to collect the data

Chicago - city of chicago shapes

**COVID** - [Chicago Data Portal covid cases](https://data.cityofchicago.org/Health-Human-Services/COVID-19-Cases-Tests-and-Deaths-by-ZIP-Code/yhhz-zm2v), aggregated weekly (uses Automatic_Weekly_Data_Generation.R [from OpenAirq-phase2/data_wrangling]; RSocrata API)

EPA_Points - outdated; all utilized EPA sensors grouped by pollutant (EPA historical)

LargeAreaCounties - 21-county shapes

NN - neural net data; updated to pre-AOD experiment base

PM25_Sensors_Monthly - outdated; EPA data pulled for May PM25 points experiment

**PM25_Weekly** - real-time weekly PM25+AQI readings from [AirNow](https://www.epa.gov/outdoor-air-quality-data/download-daily-data) (currently needs direct download from AirNow, with form for pollutant, year [2020-21], and state [IL, IN, WI for 21-county; +MI for 4-state]) [Automatic_Weekly_Data_Generation.R takes directory with a .csv for each state, as downloaded]

Tract - tract data from [tree dashboard](https://rhabus.carto.com/builder/50d25399-d7c7-4cf1-9e17-0ef44c7d7315/embed_protected); currently only tract-level PM and SVI are used in dash

ZipCodeBoundary - zip code shapes

  

### Files

**Description** - descriptions, data sources for variables; semi-integrated at the moment, but worth checking out if there’s anything unclear

**EPA_Quarterly** - updated version of EPA_Points; now also contains quarterly readings for each sensor; again from [EPA historical data](https://aqs.epa.gov/aqsweb/airdata/download_files.html)

**Master_Raster** - contains all rasters; in addition to IDW for EPA_Quarterly, contains AOD, BRDF, NDVI, land use, road density, point emissions sources, elevation, and FAA meteorological variables (temp, pressure, precip); FAA stuff is from ASOS, I have code from Lorenz on request; couldn’t find site for query

Master_Raster_Names, NN_Raster_Names - outdated, used to circumvent tif limits

NN_results - outdated, NN

PM25_Sensors_Monthly - outdated; same as above; implemented differently I guess?

Week_Index - outdated; this was a transformer file for some temporal aggregation

county_averages - similar to Master_Raster, but county aggregated; some variable differences