# Install packages if haven't already
# install.packages(c("shiny", "leaflet", "sf", "dplyr",
#                    "thematic", "data.table", "ggplot2"))

# Load in required libraries
library(shiny)
library(shinydashboard)
library(leaflet)
library(sf)
library(dplyr)
library(data.table)
library(ggplot2)
library(thematic)


library(httr)
library(jsonlite)
library(leaflet.extras)

# API Key - replace with your actual API key
api_key <- "key"

# Loads and prepares bike accident data
accidents_df <- fread("dataframe/post-processed/vision-zero-crash.csv")

# Loads and filters infrastructure data
filtered_bike_lanes <- sf::read_sf("dataframe/pre-processed/Existing_Bike_Network_2023.geojson") # nolint: line_length_linter.

# Loads Boston neighborhood boundary data
neighborhoods <- sf::read_sf("dataframe/pre-processed/Boston_Neighborhood_Boundaries_approximated_by_2020_Census_Block_Groups.geojson")


# Function to fetch and process air pollution data for a single point
fetch_pollution_data <- function(lat, lon, api_key) {
  url <- paste0("http://api.openweathermap.org/data/2.5/air_pollution?lat=", 
                lat, "&lon=", lon, "&appid=", api_key)
  response <- GET(url)
  data <- fromJSON(rawToChar(response$content))
}


ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  
  # Custom CSS to adjust the layout
  tags$head(
    tags$style(HTML("
            .title-bar {
                color: #FFF;
                background-color: #06133E;
                padding: 10px;
                text-align: center;
                font-size: 24px;
            }
            .sidebar {
                float: left;
                width: 25%;
                height: 100%;
                padding-right: 0px;
                background-color: #E9EEFF;
            }
            .main-content {
                float: right;
                width: 75%; 
            }
            .leaflet {
                height: 800px;
            }
            .checkbox-group {
                padding-left: 40px;
            }
        "))
  ),
  div(class = "title-bar", "PEDAL: Visualization for Cyclist Safety in Boston"),
  div(class = "sidebar",
      div(class = "checkbox-group",
              br(),
              br(),
              checkboxInput("bCrash", "Show Bike Accidents", value = TRUE),
              checkboxInput("bBikeLane", " Show Bike Lanes", value = FALSE),
              checkboxInput("bPM2.5", "Show PM2.5 Level", value = FALSE),
              checkboxInput("bNO2", "Show NO2 Level", value = FALSE),
              br()
      )
  ),
  div(class = "main-content",
      leafletOutput("map", width = "100%", height = "1000px")
  )
)


# Defines server logic
server <- function(input, output, session) {
  # IMPORTANT: kills the process when closing the app
  session$onSessionEnded(function() { stopApp() })

  thematic::thematic_shiny()

  # Show the modal dialog when the app starts
  showModal(modalDialog(
    title = "Welcome to PEDAL",
    "Explore bike crash data and infrastructure in Boston. 
    Use the checkboxes to toggle between crash data and bike lanes.",

    easyClose = TRUE, # When true, clicking outside the popup will close it
    # Add an 'OK' button to the modal dialog, when clicked the popup is closed
    footer = modalButton("OK!")
  ))

  # Create and render the map
  output$map <- renderLeaflet({
    # Defines coordinates for the center of Boston and as needed for aesthetics
    boston_lat <- 42.3150
    boston_long <- -71.0589
    zoom_level <- 12  # Adjust this to zoom in/out

    # A base Leaflet map centered on Boston
    map <- leaflet() %>%
      # Grayscaled map tile
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = boston_long, lat = boston_lat, zoom = zoom_level)

    # Conditionally adds bike lanes on check box of Bike Lanes
    if (input$bBikeLane) {
      map <- map %>% addPolylines(data = filtered_bike_lanes, color = "Green",
                                  weight = 3, opacity = 0.7)
    }

    # Conditionally adds crash locations on check box of Crashes
    if (input$bCrash) {
      map <- map %>% addCircleMarkers(data = accidents_df, ~long, ~lat,
                                      popup = ~as.character(dispatch_ts),
                                      color = "red", fillOpacity = 0.2,
                                      weight = 0, radius = 3)
    }
    
    # Conditionally adds neighborhood boundaries on check box of PM2.5
    if (input$bPM2.5) {
      map <- map %>% addPolygons(data = neighborhoods, color = "#0000FF",
                                 weight = 0.5, opacity = 0.5, fillOpacity = 0.2)
    }
    

    # Returns the modified map
    map
  })
}

# Creates Shiny app
shinyApp(ui, server)