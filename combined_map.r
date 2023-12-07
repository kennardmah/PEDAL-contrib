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

#--------------------TESTING AIR QUALITY----------------------------------------
library(httr)
library(jsonlite)
library(leaflet.extras)

# API Key - replace with your actual API key
api_key <- "key"

# Center coordinates
center_lat <- 42.3150
center_lon <- -71.0589
num_points <- 14  # for a 10x10 grid
offset <- 0.01  # Adjust as needed for distance between points

# Creates grid of 5 by 5 locations around center coordinates
generate_grid_coordinates <- function(center_lat, center_lon, num_points, offset) {
  grid <- expand.grid(lat = seq(from = center_lat - offset * (num_points / 2), 
                                to = center_lat + offset * (num_points / 2), 
                                by = offset),
                      lon = seq(from = center_lon - offset * (num_points / 2), 
                                to = center_lon + offset * (num_points / 2), 
                                by = offset))
  return(grid)
}

# Generate grid coordinates
coordinates <- generate_grid_coordinates(center_lat, center_lon, num_points, offset)

# Function to fetch and process air pollution data for a single point
fetch_pollution_data <- function(lat, lon, api_key) {
  url <- paste0("http://api.openweathermap.org/data/2.5/air_pollution?lat=", 
                lat, "&lon=", lon, "&appid=", api_key)
  response <- GET(url)
  data <- fromJSON(rawToChar(response$content))
  
  # Print the entire data structure
  print(data)
  
  # Check if the expected data is present
  if(!is.null(data$list) && !is.null(data$list$components) && !is.null(data$list$components$pm2_5)) {
    # Process the data and return a dataframe
    processed_data <- data.frame(
      latitude = lat,
      longitude = lon,
      pm2_5_level = data$list$components$pm2_5,  # Example for PM2.5 levels
      no2_level = data$list$components$no2  # for NO2 levels
    )
    return(processed_data)
  } else {
    # Return an empty dataframe or handle the error appropriately
    return(data.frame(latitude = numeric(0), longitude = numeric(0), pollution_level = numeric(0)))
  }
}

# Function to fetch and process air pollution data for all specified coordinates 
fetch_pollution_data_all <- function(coordinates, api_key) {
  all_data <- lapply(1:nrow(coordinates), function(i) {
    fetch_pollution_data(coordinates$lat[i], coordinates$lon[i], api_key)
  })
  do.call(rbind, all_data)
}

# Fetch and print data for all locations
# fetched_data <- fetch_pollution_data_all(coordinates, api_key)
# print("Processed Data for All Locations:")
# print(fetched_data)
#-------------------------------------------------------------------------------

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

  # Loads and prepares bike accident data
  accidents_df <- fread("dataframe/post-processed/vision-zero-crash.csv")

  # Loads and filters infrastructure data
  filtered_bike_lanes <- sf::read_sf("dataframe/pre-processed/Existing_Bike_Network_2023.geojson") # nolint: line_length_linter.

  # Show the modal dialog when the app starts
  showModal(modalDialog(
    title = "Welcome to PEDAL",
    "Explore bike crash data and infrastructure in Boston. 
    Use the checkboxes to toggle between crash data and bike lanes.",

    easyClose = TRUE, # When true, clicking outside the popup will close it
    # Add an 'OK' button to the modal dialog, when clicked the popup is closed
    footer = modalButton("OK!")
  ))
  
  # Reactive expression to fetch air pollution data
  air_pollution_data <- reactive({
    if(input$bPM2.5 | input$bNO2) {
      data <- fetch_pollution_data_all(coordinates, api_key)
      print("Air Pollution Data:")
      print(data)
      write.csv(data, "AQ_data.csv", row.names = FALSE)
      return(data)
    } else {
      return(NULL)
    }
  })

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
    
    # Conditionally adds PM heat map on check box of PM2.5 Level
    if (input$bPM2.5 && !is.null(air_pollution_data())) {
      map <- map %>% addHeatmap( data = air_pollution_data(), lng = ~longitude,
                                 lat = ~latitude, intensity = ~pm2_5_level,
                                 blur = 10, min = 6, max = 7.5, radius = 20,
                                 gradient = c(0, "green", 1, "yellow")  # Blue to red gradient
      )
    }
    
    # Conditionally adds Nitrogen dioxide heat map on check box of PM2.5 Level
    if (input$bNO2 && !is.null(air_pollution_data())) {
      map <- map %>% addHeatmap( data = air_pollution_data(), lng = ~longitude,
                                 lat = ~latitude, intensity = ~no2_level,
                                 blur = 10, min = 20, max = 30, radius = 20,
                                 gradient = c(0, "blue", 1, "red")  # Blue to red gradient
      )
    }

    # Returns the modified map
    map
  })
}

# Creates Shiny app
shinyApp(ui, server)