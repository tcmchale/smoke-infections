# shiny app

library(shiny)
library(leaflet)
library(sf)

setwd("~/Desktop/Wildfire-fungi-project/repository/smoke-infections")
covid <- read.csv("final-project/covid-19/data/covid_counties_date.csv") %>%
  rename(NAME=county)
covid$date <- lubridate::mdy(covid$date)
county <- st_read("final-project/shapefiles/grids/us_county_continental.shp") %>%
  filter(STATEFP=="06")

covid_data <- full_join(covid, county, by = "NAME")
covid_data <- st_as_sf(covid_data)
covid_data <- covid_data %>%
  filter(state=="California")

### smoke data #################
smoke_dly <- read.csv("final-project/daily-10km-smokePM/smoke_daily_10km.csv")
smoke_2020 <- filter(smoke_dly, substr(date, 1, 4) == "2020") %>%
  rename(ID = grid_id_10km)
tenkm <- st_read("final-project/shapefiles/grids/10km_grid_wgs84.shp")
county <- st_read("final-project/shapefiles/grids/us_county_continental.shp")
ca_county <- filter(county, STATEFP == "06")
#cut tenkm with california counties
tenkm_california <- st_intersection(tenkm, ca_county)
#add smoke to tenkm_ca
tenkm_casmoke <- full_join(tenkm_california, smoke_2020, by = "ID")
# Spatial join to get the county for each 10km grid cell
tenkm_casmoke_county <- st_join(tenkm_casmoke, ca_county)
tenkm_casmoke_county$date <- ymd(tenkm_casmoke_county$date)
tenkm_casmoke_county <- st_make_valid(tenkm_casmoke_county)
county_smoke <- tenkm_casmoke_county %>%
  group_by(date, NAME.x) %>%
  summarize(smoke = mean(smokePM_pred)) %>%
  ungroup() 
county_smoke_nao <- na.omit(county_smoke)  

### calculate monthly sums of covid cases and deaths ###########
covid_data_monthly <- covid_data %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(NAME, month, geometry) %>%
  summarize(cases = sum(cases), deaths = sum(deaths), .groups = "drop") %>%
  st_as_sf()


# calculate the monthly sums of county_smoke data
# Preprocess the county_smoke data
county_smoke_monthly <- county_smoke_nao %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(NAME.x, month) %>%
  summarize(smoke = sum(smoke), .groups = "drop") %>%
  st_as_sf() 


######### adding smoke to the app

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
      selectInput("selected_covid_month",
                  "Select COVID-19 Month:",
                  choices = unique(covid_data_monthly$month)),
      selectInput("selected_smoke_month",
                  "Select Smoke Exposure Month:",
                  choices = unique(county_smoke_monthly$month))
    ),
    
    # Show the maps of the selected variables
    mainPanel(
      leafletOutput("covid_map"),
      leafletOutput("smoke_map")
    )
  )
)

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
        label = ~paste0("<strong>County: </strong>", filtered_data$NAME, "<br>",
                        "<strong>", ifelse(selected_variable == "cases", "Cases: ", "Deaths: "), "</strong>",
                        format(filtered_data[[selected_variable]], big.mark = ",")),
        labelOptions = labelOptions(direction = "auto")
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
    selected_smoke_month <- input$selected_smoke_month
    
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
        label = ~paste0("<strong>County: </strong>", filtered_data$NAME, "<br>",
                        "<strong>Mean Smoke Exposure: </strong>", format(filtered_data$mean_smoke_exposure, big.mark = ",")),
        labelOptions = labelOptions(direction = "auto")
      ) %>%
      addLegend(
        pal = pal,
        values = filtered_data$smoke,
        title = "Mean Smoke Exposure",
        position = "bottomright",
        layerId = "legend"
      )
  })
}

# Run the Shiny app
shinyApp(ui, server)
