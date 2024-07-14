# COVID-19_Dashboard

Done by: Ayadurai, Medini, Roci and Yogeswaran
 
This Shiny application provides a comprehensive dashboard for monitoring COVID-19 statistics globally. It features interactive maps, tables, and plots to visualize the spread and impact of the virus across different countries.

Link to the Shiny App: https://iw3oij-mouad-medini.shinyapps.io/covid-19_dashboard/

## Table of Contents

- [Features](#features)
- [Deploy Shiny App](#deploy-shiny-app)
- [Data Sources](#data-sources)

## Features

- **Interactive Map:** Visualize COVID-19 cases, deaths, and recoveries on a global map.
- **Summary Statistics:** View total cases, deaths, and recoveries for selected countries.
- **Data Table:** Explore detailed COVID-19 data in a tabular format.
- **Time Series Analysis:** Plot confirmed cases, recovered cases, and deaths over time for selected countries.
- **Country Selection:** Filter data and visualizations by specific countries.

## Deploy Shiny App

1. Create Account
2. Install rsconnect package:
- install.packages('rsconnect')
- library('rsconnect')
4. Login to account:
- rsconnect::setAccountInfo(name='USERNAME',  token='TOKEN',  secret='SECRET')
5. Deploy app:
- rsconnect::deployApp('./COVID-19_Dashoboard')

## Data Sources

Used data for the Shiny application are taken from the following sources:

- https://www.worldometers.info/coronavirus/
- https://www.kaggle.com/datasets/paultimothymooney/latitude-and-longitude-for-every-country-and-state?resource=download
- https://www.kaggle.com/datasets/niketchauhan/covid-19-time-series-data
