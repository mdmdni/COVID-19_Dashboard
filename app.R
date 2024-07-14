library(shiny)
library(shinydashboard)
library(dplyr)
library(rvest)
library(httr)
library(xml2)
library(leaflet)
library(readr)
library(DT)
library(plotly)

# Define the URL of the website
url <- "https://www.worldometers.info/coronavirus/"

# Read the HTML contents of the website
html_content <- read_html(url)

# Import Coordinates Data for leaflet map
coordinates <- read.csv("world_country_and_usa_states_latitude_and_longitude_values.csv")

# Extract the table and filter the data
table <- html_content %>%
  html_node(xpath = '//*[@id="main_table_countries_today"]') %>%
  html_table()

# Clean and filter the table
filtered_table <- table %>%
  filter(`#` != "") %>%
  select(`#`, `Country,Other`, TotalCases, TotalDeaths, TotalRecovered) %>%
  rename(country = `Country,Other`)

# Convert columns to numeric, remove commas, and handle non-numeric entries
filtered_table <- filtered_table %>%
  mutate(
    TotalCases = parse_number(TotalCases),
    TotalDeaths = parse_number(TotalDeaths),
    TotalRecovered = if_else(TotalRecovered == "N/A", NA_character_, TotalRecovered) %>%
      parse_number()
  )

# Replace country names to match the CSV file
country_replacements <- c(
  "USA" = "United States",
  "S. Korea" = "South Korea",
  "UK" = "United Kingdom",
  "DPRK" = "North Korea",
  "Czechia" = "Czech Republic",
  "UAE" = "United Arab Emirates",
  "Myanmar" = "Myanmar [Burma]",
  "Palestine" = "Israel",
  "North Macedonia" = "Macedonia [FYROM]",
  "Channel Islands" = "Guernsey",
  "DRC" = "Congo [DRC]",
  "Ivory Coast" = "Côte d'Ivoire",
  "Eswatini" = "Swaziland",
  "Cabo Verde" = "Cape Verde",
  "Faeroe Islands" = "Faroe Islands",
  "Macao" = "Macau",
  "Congo" = "Congo [Republic]",
  "South Sudan" = "Sudan",
  "CAR" = "Central African Republic",
  "Caribbean Netherlands" = "Netherlands Antilles",
  "St. Vincent Grenadines" = "Saint Vincent and the Grenadines",
  "Turks and Caicos" = "Turks and Caicos Islands",
  "Saint Pierre Miquelon" = "Saint Pierre and Miquelon"
)

filtered_table <- filtered_table %>%
  mutate(country = recode(country, !!!country_replacements))

filtered_coordinates <- coordinates %>%
  select(longitude, latitude, country)

# Join data and handle possible join errors
joined_data <- left_join(filtered_table, filtered_coordinates, by = "country")

# Remove rows with NA values in coordinates
cleaned_data <- joined_data %>% filter(!is.na(longitude) & !is.na(latitude))

# Fetch time series data from GitHub
time_series_url <- "https://raw.githubusercontent.com/datasets/covid-19/master/data/time-series-19-covid-combined.csv"
time_series_data <- read_csv(time_series_url)

# Ensure column names are correct
print(colnames(time_series_data))

# UI
ui <- dashboardPage(
  dashboardHeader(title = "COVID-19 Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Time Series", tabName = "time_series", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                column(width = 4,
                       box(title = "Select Country", status = "primary", solidHeader = TRUE, width = 12,
                           selectizeInput("selected_country", "Select Countries:", choices = c("World", unique(cleaned_data$country)), selected = "World", multiple = TRUE, options = list(placeholder = 'Select countries', dropdownParent = 'body'))
                       )
                ),
                column(width = 8,
                       box(title = "COVID-19 Map", status = "primary", solidHeader = TRUE, width = 12,
                           leafletOutput("covid_map", height = 500))
                )
              ),
              fluidRow(
                column(width = 4,
                       box(title = "Summary Statistics", status = "primary", solidHeader = TRUE, width = 12,
                           verbatimTextOutput("summary_stats")
                       )
                ),
                column(width = 8,
                       box(title = "COVID-19 Data", status = "primary", solidHeader = TRUE, width = 12,
                           DT::dataTableOutput("covid_table"))
                )
              )
      ),
      tabItem(tabName = "time_series",
              fluidRow(
                column(width = 6,
                       box(title = "Select Country", status = "primary", solidHeader = TRUE, width = 12,
                           selectizeInput("selected_countries_ts", "Select Countries:", choices = unique(time_series_data$`Country/Region`), selected = "US", multiple = TRUE, options = list(placeholder = 'Select countries', dropdownParent = 'body'))
                       )
                ),
                column(width = 6,
                       box(title = "Select Date Range", status = "primary", solidHeader = TRUE, width = 12,
                           dateRangeInput("date_range", "Select Date Range:", start = min(time_series_data$Date), end = max(time_series_data$Date), min = min(time_series_data$Date), max = max(time_series_data$Date))
                       )
                )
              ),
              fluidRow(
                column(width = 4,
                       box(title = "Confirmed Cases Plot", status = "primary", solidHeader = TRUE, width = 12,
                           plotlyOutput("confirmed_cases_plot", height = 400))
                ),
                column(width = 4,
                       box(title = "Recovered Cases Plot", status = "primary", solidHeader = TRUE, width = 12,
                           plotlyOutput("recovered_cases_plot", height = 400))
                ),
                column(width = 4,
                       box(title = "Deaths Plot", status = "primary", solidHeader = TRUE, width = 12,
                           plotlyOutput("deaths_plot", height = 400))
                )
              ),
              fluidRow(
                column(width = 12,
                       box(title = "Time Series Data", status = "primary", solidHeader = TRUE, width = 12,
                           DT::dataTableOutput("time_series_table"))
                )
              )
      )
    ),
    # Footer
    div(
      class = "footer",
      p("© Ayadurai, Medini, Roci, and Yogeswaran. ",
        a(href = "https://github.com/mdmdni/COVID-19_Dashboard.git", "GitHub Repository")
      ),
      style = "text-align: center; padding: 10px; background-color: #f8f9fa; border-top: 1px solid #e9ecef;")
  )
)

# Server
server <- function(input, output, session) {
  selected_data <- reactive({
    if ("World" %in% input$selected_country) {
      cleaned_data
    } else {
      cleaned_data %>% filter(country %in% input$selected_country)
    }
  })
  
  output$summary_stats <- renderPrint({
    data <- selected_data()
    total_cases <- sum(data$TotalCases, na.rm = TRUE)
    total_deaths <- sum(data$TotalDeaths, na.rm = TRUE)
    total_recovered <- sum(data$TotalRecovered, na.rm = TRUE)
    cat("Summary Statistics for Selected Countries:\n")
    cat("Total Cases: ", total_cases, "\n")
    cat("Total Deaths: ", total_deaths, "\n")
    cat("Total Recovered: ", total_recovered, "\n")
  })
  
  output$covid_table <- DT::renderDataTable({
    req(selected_data())
    DT::datatable(selected_data())
  })
  
  output$covid_map <- renderLeaflet({
    leaflet() %>%
      addTiles()
  })
  
  observeEvent(input$selected_country, {
    data <- selected_data()
    leafletProxy("covid_map") %>%
      clearMarkers() %>%
      clearShapes()
    
    if (nrow(data) > 0) {
      leafletProxy("covid_map") %>%
        addCircles(lng = data$longitude, lat = data$latitude,
                   weight = 1, radius = sqrt(data$TotalCases) * 100,
                   popup = paste(data$country, "<br>",
                                 "Total Cases: ", data$TotalCases, "<br>",
                                 "Total Deaths: ", data$TotalDeaths, "<br>",
                                 "Total Recovered: ", data$TotalRecovered))
      
      if ("World" %in% input$selected_country || length(input$selected_country) > 1) {
        leafletProxy("covid_map") %>%
          fitBounds(lng1 = min(data$longitude), lat1 = min(data$latitude),
                    lng2 = max(data$longitude), lat2 = max(data$latitude))
      } else {
        leafletProxy("covid_map") %>%
          setView(lng = data$longitude[1], lat = data$latitude[1], zoom = 4)
      }
    }
  })
  
  selected_time_series_data <- reactive({
    req(input$date_range)
    time_series_data %>% 
      filter(`Country/Region` %in% input$selected_countries_ts,
             Date >= input$date_range[1],
             Date <= input$date_range[2])
  })
  
  output$time_series_table <- DT::renderDataTable({
    req(selected_time_series_data())
    DT::datatable(selected_time_series_data())
  })
  
  output$confirmed_cases_plot <- renderPlotly({
    req(selected_time_series_data())
    plot_ly(selected_time_series_data(), x = ~Date, y = ~Confirmed, type = 'scatter', mode = 'lines', color = ~`Country/Region`) %>%
      layout(title = "Confirmed Cases", xaxis = list(title = "Date"), yaxis = list(title = "Confirmed Cases"))
  })
  
  output$recovered_cases_plot <- renderPlotly({
    req(selected_time_series_data())
    plot_ly(selected_time_series_data(), x = ~Date, y = ~Recovered, type = 'scatter', mode = 'lines', color = ~`Country/Region`) %>%
      layout(title = "Recovered Cases", xaxis = list(title = "Date"), yaxis = list(title = "Recovered Cases"))
  })
  
  output$deaths_plot <- renderPlotly({
    req(selected_time_series_data())
    plot_ly(selected_time_series_data(), x = ~Date, y = ~Deaths, type = 'scatter', mode = 'lines', color = ~`Country/Region`) %>%
      layout(title = "Deaths", xaxis = list(title = "Date"), yaxis = list(title = "Deaths"))
  })
}

# App
shinyApp(ui, server)
