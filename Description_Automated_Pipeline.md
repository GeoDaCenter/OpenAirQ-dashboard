# Overview of the Automated Data Scraping Pipeline and Data Wrangling Files

This project contains two automated data scraping pipelines powered by the GitHub Actions features:

- Air-Quality-Data-Scraping: This pipeline is scheduled to run everyday at 7 am central time (12 pm UTC) to automatically scrape the most recent air quality index and PM<sub>2.5</sub> data from the [AirNow API](https://www.airnow.gov/) maintained by EPA. This pipleline is controlled by the script `Air-Quality-Data-Scraping.yml` in `.github/workflows` and uses the script `Air_Quality_Scraping.R` in the folder `Data_Wrangling`. For each variable of interest, the R data wrangling script creates in the folder `Data/PM25_Weekly` a wide dataset of daily values (`aqi.csv` or `pm25.csv`) and a dataset of aggregated weekly values across all sites (`aqi_means.csv` or `pm25_means.csv`).

- Covid-Data-Scraping: This pipeline is scheduled to run every Monday at 7 am central time (12 pm UTC) to automatically scrape the most recent data of zipcode-level Covid cases from the [data portal of City of Chicago](https://data.cityofchicago.org/). This pipleline is controlled by the script `Covid-Data-Scraping.yml` in `.github/workflows` and uses the script `Covid_Scraping.R` in the folder `Data_Wrangling`. The R data wrangling script creates in the folder `Data/COVID` a wide dataset of weekly Covid cases at different zipcodes (`CovidWeekly.csv`), a dataset of aggregated mean weekly cases across all localities (`covid_means.csv`) and an updated geojson file (`covid.geojson`).

In addition to the two scripts used in the automated pipeline, there is another script `Cleaning_Existing_Data.R` in the folder `Data_Wrangling` that processes and merges the historical data in the folder `Data/PM25_Weekly/Historical`. This file only ran once locally and is not part of the pipeline once we have finished processed and merged historical data into new datasests that are periodically updated by the two pipelines above.

# Notes

- For more technical details about Github Actions Features: https://github.com/features/actions
- For setting up your own AirNow API keys as a GitHub secret: https://docs.github.com/en/actions/security-guides/encrypted-secrets