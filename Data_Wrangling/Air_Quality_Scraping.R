library(dplyr)
library(tidyr)
library(jsonlite)
library(stringr)
library(lubridate)
library(bigrquery)
library(gargle)
library(readr)

# BigQuery setup

path_to_data <- "Data/PM25_Weekly/"
json_string = Sys.getenv("BQ_key")
auth_email = Sys.getenv("BQ_user")
bq_auth(email = auth_email,
        path = json_string)
# project set up
project <- "open-airq-bigquery" # replace this with your project ID 
sql_aqi <- "SELECT * FROM Scraped_Data.AQI"
sql_pm25 <- "SELECT * FROM Scraped_Data.PM25"
options(
  gargle_oauth_email = auth_email
)
# read in existing data
pm25_old <- bq_project_query(project, sql_pm25) %>% 
  bq_table_download() %>% 
  mutate(across(contains("PM25"), as.numeric)) %>% 
  select(Site_ID, COUNTY, latitude, longitude, name, everything())

aqi_old <- bq_project_query(project, sql_aqi) %>% 
  bq_table_download() %>% 
  mutate(across(contains("AQI"), as.numeric)) %>% 
  select(Site_ID, COUNTY, latitude, longitude, name, everything()) 

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
  rename(Site_ID = `Site ID`) %>% 
  right_join(., pm25_old, by ='Site_ID') %>% 
  relocate(c("COUNTY", "latitude", "longitude", "name"), 
           .after = 'Site_ID') 

write.csv(pm_combo, file = paste0(path_to_data, "pm25.csv"),
          row.names = F)

aqi_combo <- aqi_wide %>% 
  mutate(`Site ID` = as.numeric(`Site ID`)) %>% 
  rename(Site_ID = `Site ID`) %>% 
  right_join(., aqi_old, by ='Site_ID') %>% 
  relocate(c("COUNTY", "latitude", "longitude", "name"), 
           .after = 'Site_ID')

write.csv(aqi_combo, file = paste0(path_to_data, "aqi.csv"),
          row.names = F)

upload_bq_table <- function(table_name, df){
  # this function handles table overwriting in BQ
  bq_tb <- bq_table(project, dataset = "Scraped_Data", 
                    table = table_name)
  bq_table_delete(bq_tb)
  bq_tb <- bq_table_create(bq_tb, df)
  bq_table_upload(bq_tb, df)
}

upload_bq_table("AQI", aqi_combo)
upload_bq_table("PM25", pm_combo)

# aggregate to weekly data

aggregate_weekly <- function(df, var){
  # this function aggregates data to weekly value
  var_weekly_means <- df %>% 
    select(everything(), 
           -c("COUNTY", "latitude", "longitude", "name")) %>%
    pivot_longer(col = -Site_ID, names_to = 'date', 
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

upload_bq_table("AQI_means", aqi_weekly)
upload_bq_table("PM25_means", pm_weekly)

write.csv(aqi_weekly, file = paste0(path_to_data, "aqi_means.csv"),
          row.names = F)

write.csv(pm_weekly, file = paste0(path_to_data, "pm25_means.csv"),
          row.names = F)


