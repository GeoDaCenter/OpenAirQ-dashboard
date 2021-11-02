library(dplyr)
library(tidyr)
library(jsonlite)
library(stringr)
library(lubridate)


#setwd('E:/Spatial DS RA/Action-Testing/Data_Wrangling/')
path_to_data <- "Data/PM25_Weekly/"

# pm25: combine wide data
pm25_old <- read.csv(paste0(path_to_data, "pm25.csv"))
# aqi: combine wide data
aqi_old <- read.csv(paste0(path_to_data, "aqi.csv"))

# setup
# Read in the most recent day to identify the start day
most_recent_date <- str_extract(names(aqi_old), "[0-9]{3,}") %>% 
  na.omit() %>% first() %>% ymd()
last_day = most_recent_date + 1
up_to_day <- as.character(Sys.Date())
api_key <- Sys.getenv("api_key")


# the AirNow API uses UTC time: UTC 5 am = CDT 12 am
make_query_url <- function(last_day, today, key){
  # build query for data downloading
  base <- 'https://www.airnowapi.org/aq/data/?'
  start <- paste0("startDate=", last_day, "T05")
  end <- paste0("&endDate=", today, "T04")
  paramters <- '&parameters=PM25'
  bbox <- '&BBOX=-88.77879,40.73641,-86.46629,43.21831'
  data_type <- '&dataType=B&format=application/json'
  others <- '&verbose=1&monitorType=2&includerawconcentrations=1'
  api_key <- paste0('&API_KEY=', key)
  query_v <- c(base, start, end, paramters, bbox, data_type, others,
               api_key)
  final_query <- paste(query_v, collapse = "")
}

data_url <- make_query_url(last_day, up_to_day, api_key)

# ------ process newly acquired data
new_dt <- fromJSON(url(data_url))

aggregate_day <- function(data){
  # process data by date
  data <- data %>% 
    rename(`Site ID` = FullAQSCode) %>%
    mutate(is_last_day = str_detect(UTC, pattern = 'T0[0-4]'),
           date = as.Date(UTC),
           date = if_else(is_last_day, date - 1 , date)) %>% 
    filter(date >= "2021-03-11")
  
  data_agg <- data %>% 
    group_by(`Site ID`, date) %>% 
    summarize(date = first(date),
              avg_pm25 = mean(Value),
              avg_aqi = mean(AQI)) %>% 
    mutate(date = str_remove_all(date, "-")) %>% 
    arrange(desc(date))
  return(data_agg)
} 

dt_agg <- aggregate_day(new_dt)

# ----- reshape to wide 
pm_wide <- dt_agg %>% 
  select(-avg_aqi) %>% 
  pivot_wider(id_cols = `Site ID`, 
              names_from = date, 
              values_from = avg_pm25, 
              names_prefix = "PM25_")

aqi_wide <- dt_agg %>% 
  select(-avg_pm25) %>% 
  pivot_wider(id_cols = `Site ID`, 
              names_from = date, 
              values_from = avg_aqi, 
              names_prefix = "AQI_")

# ------ combine new data with existing dataset

pm_combo <- pm_wide %>% 
  mutate(`Site ID` = as.numeric(`Site ID`)) %>% 
  rename(Site.ID = `Site ID`) %>% 
  right_join(., pm25_old, by ='Site.ID') %>% 
  relocate(c("COUNTY", "latitude", "longitude", "name"), 
           .after = 'Site.ID')

write.csv(pm_combo, file = paste0(path_to_data, "pm25.csv"), 
          row.names = F)

aqi_combo <- aqi_wide %>% 
  mutate(`Site ID` = as.numeric(`Site ID`)) %>% 
  rename(Site.ID = `Site ID`) %>% 
  right_join(., aqi_old, by ='Site.ID') %>% 
  relocate(c("COUNTY", "latitude", "longitude", "name"), 
           .after = 'Site.ID')

write.csv(aqi_combo, file = paste0(path_to_data, "aqi.csv"), 
          row.names = F)
