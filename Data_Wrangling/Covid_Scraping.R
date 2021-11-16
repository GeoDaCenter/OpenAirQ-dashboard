library(tidyr)
library(dplyr)
library(RSocrata)
library(geojsonio)
library(bigrquery)
library(gargle)
library(readr)
library(jsonlite)
# This scripts scrape up-to-date Covid Data

### Scrape COVID data from Chicago data Portal
covid_chicago <- read.socrata("https://data.cityofchicago.org/resource/yhhz-zm2v.json")

### Calculate 7-day average total case
### Can get hospitality rate as well
covid_raw <- covid_chicago %>%
  rename(Date = week_start) %>%
  filter(Date >= '2020-12-01') %>%
  arrange(desc(Date)) %>%
  dplyr::select(zip_code, week_end, case_rate_weekly) %>% 
  mutate(case_rate_weekly = as.numeric(case_rate_weekly)) %>% 
  rename(Date = week_end) %>% 
  pivot_wider(names_from = 'Date', values_from = 'case_rate_weekly') %>%
  rename(zip = zip_code) %>% 
  filter(zip != "Unknown")

# clean up variable names
zipcode <- covid_raw$zip
covid <- dplyr::select(covid_raw, -zip)
colnames(covid) <- gsub("-", "", colnames(covid))
colnames(covid) <- paste0('COVID_Week_', colnames(covid))
covid <- cbind(zipcode, covid)

write.csv(covid, file = "Data/COVID/CovidWeekly.csv",
          row.names = F)

# calculate mean covid cases across all localities
covid_means <- covid %>% 
  pivot_longer(col = -zipcode, names_to = 'time', 
               values_to = "count") %>% 
  group_by(time) %>% 
  mutate(count = as.numeric(count)) %>%
  summarize(mean_covid = mean(count, na.rm = T)) %>% 
  arrange(desc(time))

write.csv(covid_means, file = "Data/COVID/covid_means.csv",
          row.names = F)

# update the data tables stored in Google Cloud

## setup bigquery
json_string = Sys.getenv("BQ_key")
auth_email = Sys.getenv("BQ_user")
bq_auth(email = auth_email,
        path = json_string)

project <- "open-airq-bigquery"
options(
  gargle_oauth_email = auth_email
)

## upload to bigquery
upload_bq_table <- function(table_name, df){
  # this function handles table overwriting in BQ
  bq_tb <- bq_table(project, dataset = "Scraped_Data", 
                    table = table_name)
  bq_table_delete(bq_tb)
  bq_tb <- bq_table_create(bq_tb, df)
  bq_table_upload(bq_tb, df)
}

upload_bq_table("CovidWeekly", covid)
upload_bq_table("Covid_means", covid_means)

# update the geojson file
covid_geo <- geojson_read("Data/COVID/historical/covid.geojson",
                          what = "sp")

covid_geo@data <- covid_geo@data %>%
  select(all_of(names(covid_geo@data)[1:5])) %>%
  right_join(., covid, by = c("zip" = "zipcode"))

geojson_write(covid_geo, file = "Data/COVID/covid.geojson")
