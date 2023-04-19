#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(dplyr)
library(shiny)
library(leaflet)
library(sf)
library(here)
library(lubridate)

# Define the file paths relative to the app directory
covid_data_monthly_file <- here( "shapefiles", "covid_data_monthly.shp")
county_smoke_monthly_file <- here("shapefiles", "county_smoke_monthly_ca.shp")
# Define the months of the year
months_of_year <- c("January", "February", "March", "April", "May", "June",
                    "July", "August", "September", "October", "November", "December")

# Load the data
covid_data_monthly <- st_read(covid_data_monthly_file)
county_smoke_monthly <- st_read(county_smoke_monthly_file) 


# Define the UI
ui <- fluidPage(
  # Title panel
  titlePanel("COVID-19 and Smoke Exposure in California by County (2020)"),
  
  # Sidebar with select inputs for cases or deaths, and month for each dataset
  sidebarLayout(
    sidebarPanel(
      selectInput("variable",
                  "Select COVID-19 Variable:",
                  choices = c("Cases" = "cases", "Deaths" = "deaths")),
      # Update the selectInput choices with sorted months
      selectInput("selected_covid_month",
                  "Select COVID-19 Month:",
                  choices = sort(unique(covid_data_monthly$month))),
      
      selectInput("selected_smoke_month",
                  "Select Smoke Exposure Month:",
                  choices = sort(unique(county_smoke_monthly$month)))
    ),
    # Show the maps of the selected variables
    mainPanel(
      leafletOutput("covid_map"),
      leafletOutput("smoke_map")
    )))

# Define the server logic
server <- function(input, output, session) {
  # Render the COVID-19 map
  output$covid_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -119.4179, lat = 36.7783, zoom = 6)
  })
  
  # Update the COVID-19 map based on the selected variable and month
  observe({
    selected_variable <- input$variable
    selected_covid_month <- input$selected_covid_month
    
    # Filter the dataset based on the selected variable and month
    filtered_data <- covid_data_monthly[covid_data_monthly$month == selected_covid_month, ]

    pal <- colorNumeric("viridis", filtered_data[[selected_variable]])
    
    leafletProxy("covid_map", data = filtered_data) %>%
      clearShapes() %>%
      addPolygons(
        fillColor = ~pal(filtered_data[[selected_variable]]),
        color = "#BDBDC3",
        weight = 1,
        opacity = 1,
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          color = "white",
          weight = 2,
          bringToFront = TRUE
        ),
        label = paste0(filtered_data$NAME, 
                       "; ",
                       ifelse(selected_variable == "cases", "Cases: ", "Deaths: "),
                       format(filtered_data[[selected_variable]], big.mark = ",")),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal"),
          textsize = "15px",
          direction = "auto"
        ),
        group = "covid_counties"
      ) %>%
      addLegend(
        pal = pal,
        values = filtered_data[[selected_variable]],
        title = ifelse(selected_variable == "cases", "Cases", "Deaths"),
        position = "bottomright",
        layerId = "legend"
      )
  })
  
  # Render the smoke exposure map
  output$smoke_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -119.4179, lat = 36.7783, zoom = 6)
  })
  
  # Update the smoke exposure map based on the selected month
  observe({
    selected_smoke_month <-  input$selected_smoke_month
    
    # Filter the dataset based on the selected month
    filtered_data <- county_smoke_monthly[county_smoke_monthly$month == selected_smoke_month, ] 

    pal <- colorNumeric("viridis", filtered_data$smoke)
    
    leafletProxy("smoke_map", data = filtered_data) %>%
      clearShapes() %>%
      addPolygons(
        fillColor = ~pal(filtered_data$smoke),
        color = "#BDBDC3",
        weight = 1,
        opacity = 1,
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          color = "white",
          weight = 2,
          bringToFront = TRUE
        ),
        label = paste0(filtered_data$NAME, 
                       "; ",
                       "\n Mean Smoke Exposure: ", format(round(filtered_data$smoke, 2), big.mark = ",")),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal"),
          textsize = "15px",
          direction = "auto"
        ),
        group = "covid_counties"
      ) %>%
      addLegend(
        pal = pal,
        values = filtered_data$smoke,
        title = "Mean Smoke Exposure, PM2.5 (µg/m^3)",
        position = "bottomright",
        layerId = "legend"
      )
  })
}

# Run the Shiny app
shinyApp(ui, server)
