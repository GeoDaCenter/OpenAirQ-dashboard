library(dplyr)
library(tidyr)
library(purrr)
library(jsonlite)
library(stringr)
library(lubridate)

setwd("E:/Spatial DS RA/OpenAirQ-dashboard/Data/PM25_Weekly/Historical/")
select <- dplyr::select
#---read and process newly acquired data since march
past_files <- list.files(pattern="*.json")

process <- function(f_name){
  
  data <- fromJSON(f_name) %>% 
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
    mutate(date = str_remove_all(date, "-")) 
  return(data_agg)
} 

past_combined <- map_dfr(.x=past_files, .f = ~process(.x))

past_combined <- 
  past_combined %>% 
  filter(date != "20211028") 
# ------separate into different datasets
# pm25
pm_wide <- past_combined %>% 
  arrange(desc(date)) %>% 
  select(-avg_aqi) %>% 
  pivot_wider(id_cols = `Site ID`, 
              names_from = date, 
              values_from = avg_pm25, 
              names_prefix = "PM25_")

# aqi
aqi_wide <- past_combined %>% 
  arrange(desc(date)) %>% 
  select(-avg_pm25) %>% 
  pivot_wider(id_cols = `Site ID`, 
              names_from = date, 
              values_from = avg_aqi, 
              names_prefix = "AQI_")

# weekly data
pm_weekly <- past_combined %>%
  ungroup() %>%
  select(date, avg_pm25) %>%
  mutate(week = week(ymd(date))) %>%
  group_by(week) %>%
  summarize(PM25 = mean(avg_pm25, na.rm = T)) %>%
  select(PM25)


aqi_weekly <- past_combined %>%
  ungroup() %>%
  select(date, avg_aqi) %>%
  mutate(week = week(ymd(date))) %>%
  group_by(week) %>%
  summarize(AQI = mean(avg_aqi, na.rm = T)) %>%
  select(AQI)

# merge with previous data

# pm25: combine weekly data
pm_weekly_old <- read.csv("pm25_means.csv")
pm_weekly_combo <- rbind(pm_weekly, pm_weekly_old)
write.csv(pm_weekly_combo,
          file = "../pm25_means.csv")

# aqi: combine weekly data
aqi_weekly_old <- read.csv("aqi_means.csv")

aqi_weekly_combo <- rbind(aqi_weekly, aqi_weekly_old)
write.csv(aqi_weekly_combo,
          file = "../aqi_means.csv")

# pm25: combine wide data
pm25_old <- read.csv("pm25.csv")

pm_combo <- pm_wide %>% 
  mutate(`Site ID` = as.numeric(`Site ID`)) %>% 
  rename(Site.ID = `Site ID`) %>% 
  right_join(., pm25_old, by ='Site.ID') %>% 
  relocate(c("COUNTY", "latitude", "longitude", "name"), 
           .after = 'Site.ID') %>% 
  select(-c('X'))

write.csv(pm_combo, file = "../pm25.csv", row.names = F)

# aqi: combine wide data
aqi_old <- read.csv("aqi.csv")

aqi_combo <- aqi_wide %>% 
  mutate(`Site ID` = as.numeric(`Site ID`)) %>% 
  rename(Site.ID = `Site ID`) %>% 
  right_join(., aqi_old, by ='Site.ID') %>% 
  relocate(c("COUNTY", "latitude", "longitude", "name"), 
           .after = 'Site.ID') %>% 
  select(-c('X'))

write.csv(aqi_combo, file = "../aqi.csv", row.names = F)
