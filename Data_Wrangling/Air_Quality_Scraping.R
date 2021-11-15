library(dplyr)
library(tidyr)
library(jsonlite)
library(stringr)
library(lubridate)

path_to_data <- "Data/PM25_Weekly/"

# read in existing data
pm25_old <- read.csv(paste0(path_to_data, "pm25.csv"))
aqi_old <- read.csv(paste0(path_to_data, "aqi.csv"))

# identify the most recent day for which data is available
most_recent_date <- str_extract(names(aqi_old), "[0-9]{3,}") %>% 
  na.omit() %>% first() %>% ymd()
last_day = most_recent_date + 1
up_to_day <- as.character(Sys.Date())
api_key <- Sys.getenv("api_key")

# identify time difference between utc and chicago time
current_time_ct <- Sys.time()
current_time_utc <- ymd_hms(with_tz(current_time_ct, tz = "UTC"), 
                            tz = "America/Chicago")
diff <- difftime(current_time_utc, current_time_ct, units="hours") %>% 
  as.numeric() %>% 
  round(., 1)

# the AirNow API uses UTC time, so we need to adjust for the difference
# at different season
make_query_url <- function(last_day, today, key, diff){
  # build query for data downloading
  base <- 'https://www.airnowapi.org/aq/data/?'
  
  start_hour <- paste0("T0", diff)
  end_hour <- paste0("T0", as.numeric(diff - 1))
  
  start <- paste0("startDate=", last_day, start_hour)
  end <- paste0("&endDate=", today, end_hour)
  paramters <- '&parameters=PM25'
  bbox <- '&BBOX=-88.77879,40.73641,-86.46629,43.21831'
  data_type <- '&dataType=B&format=application/json'
  others <- '&verbose=1&monitorType=2&includerawconcentrations=1'
  api_key <- paste0('&API_KEY=', key)
  query_v <- c(base, start, end, paramters, bbox, data_type, others,
               api_key)
  final_query <- paste(query_v, collapse = "")
  return(final_query)
}

# generate query url
data_url <- make_query_url(last_day, up_to_day, api_key, diff)

# ------ process newly acquired data
new_dt <- fromJSON(url(data_url))

aggregate_day <- function(data, diff){
  # process data by date
  # identify the last hour of the day
  end_hour <- diff - 1
  hour_pattern <- paste0("T0[0-", end_hour, "]") 
  data <- data %>% 
    rename(`Site ID` = FullAQSCode) %>%
    mutate(is_last_day = str_detect(UTC, pattern = hour_pattern),
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

dt_agg <- aggregate_day(new_dt, diff)

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

# aggregate to weekly data

aggregate_weekly <- function(df, var){
  # this function aggregates data to weekly value
  var_weekly_means <- df %>% 
    select(everything(), 
           -c("COUNTY", "latitude", "longitude", "name")) %>%
    pivot_longer(col = -Site.ID, names_to = 'date', 
                 values_to = "value") %>%
    mutate(date = str_extract(date, "[0-9]{3,}"),
           date = ymd(date),
           week = week(date),
           year = year(date),
           value = as.numeric(value)) %>% 
    group_by(year, week) %>% 
    summarize(var = mean(value, na.rm = T)) %>% 
    arrange(desc(year), desc(week)) %>% 
    ungroup() %>% 
    select(var) 
  
  return(var_weekly_means)
}

# output weekly data
aqi_weekly <- aggregate_weekly(aqi_combo, AQI) %>% 
  rename(AQI = var)

pm_weekly <- aggregate_weekly(pm_combo, PM25) %>% 
  rename(PM25 = var)

write.csv(aqi_weekly, file = paste0(path_to_data, "aqi_means.csv"), 
          row.names = F)

write.csv(pm_weekly, file = paste0(path_to_data, "pm25_means.csv"), 
          row.names = F)


