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
library(thematic)
library(dplyr)
library(RColorBrewer)
library(httr)
library(jsonlite)
library(leaflet.extras)

# Loads and prepares bike accident data
accidents_df <- fread("dataframe/post-processed/vision-zero-crash.csv")

# Loads and filters infrastructure data
filtered_bike_lanes <- sf::read_sf("dataframe/post-processed/Existing_Bike_Network_2023.geojson")

# API Key - replace with your actual API key
api_key <- "9cf634461389fa0f5ab332e7c49f22ad"

# Path to your GeoJSON file
file_path <- "dataframe/pre-processed/Boston_Neighborhood_Boundaries_approximated_by_2020_Census_Block_Groups.geojson"

# Read the GeoJSON file
geo_data <- st_read(file_path)

# Extract only the name and geometry
processed_data <- geo_data %>%
  select(blockgr2020_ctr_neighb_name, geometry)

# Calculate centroids of the geometries
centroids <- st_centroid(processed_data$geometry)

# Extract longitude and latitude from centroids
centroid_data <- data.frame(
  name = processed_data$blockgr2020_ctr_neighb_name,
  geomtery = processed_data$geometry,
  centre_lon = st_coordinates(centroids)[, 1],
  centre_lat = st_coordinates(centroids)[, 2]
)

# Function to fetch and process air pollution data for a single point
fetch_pollution_data <- function(lat, lon, api_key) {
  url <- paste0("http://api.openweathermap.org/data/2.5/air_pollution?lat=",
                lat, "&lon=", lon, "&appid=", api_key)
  response <- GET(url)
  data <- fromJSON(rawToChar(response$content))
  
  if(!is.null(data$list) && !is.null(data$list$components) && !is.null(data$list$components$pm2_5)) {
    processed_data <- data.frame(
      centre_lat = lat,
      centre_lon = lon,
      pm2_5_level = data$list$components$pm2_5,
      no2_level = data$list$components$no2
    )
    return(processed_data)
  } else {
    return(data.frame(latitude = numeric(0), longitude = numeric(0), pollution_level = numeric(0)))
  }
}

# Fetches AQ data for all the neighborhood centroid points
fetch_pollution_data_all <- function(centroid_data, api_key) {
  all_data <- lapply(1:nrow(centroid_data), function(i) {
    fetch_pollution_data(centroid_data$centre_lat[i], centroid_data$centre_lon[i], api_key)
  })
  do.call(rbind, all_data)
}

air_quality_results <- fetch_pollution_data_all(centroid_data, api_key)

merged_final_data <- merge(centroid_data, air_quality_results, by = c("centre_lat", "centre_lon"))

final_data_sf <- st_as_sf(merged_final_data, sf_column_name = "geometry")

final_data_sf <- final_data_sf %>%
  arrange(name)

# UI Definition
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  
  tags$head(
    tags$style(HTML("
            .title-bar {
                color: #FFF;
                background-color: #06133E;
                opacity: 1;
                padding: 20px;
                text-align: center;
                font-size: 24px;
            }
            .title-image {
                width: 10%;
                padding: 10px 0;
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
            .sidebar-image {
                width: 100%;
                padding: 10px 0;
            }
            .contribute-github {
              position: fixed;
              bottom: 25px;
              right: 25px;
              z-index: 100;
            }
            .github-image {
              height: 60px;
              width: 60px;
              margin-right: 5px;
            }
            .github-text {
              color: black;
              font-size: 14px;
              font-weight: bold;
              text-decoration: none;
            }
        "))
  ),
  div(class = "title-bar", 
      img(src = "pedal_white.png", class = "title-image"), ": Visualisation for Cyclist Decision-Making in Boston"),
  div(class = "sidebar",
      div(class = "checkbox-group",
          br(),
          br(),
          radioButtons("dataLayer", "Data Layers:",
                       choices = list("Show Bike Accidents" = "bCrash",
                                      "Show Bike Lanes" = "bBikeLane",
                                      "Show PM2.5 Level" = "bPM2.5",
                                      "Show NO2 Level" = "bNO2"),
                       selected = NULL),
          br()
      ),
      img(src = "legend.png", class = "sidebar-image"),
  ),
  div(class = "main-content",
      leafletOutput("map", width = "100%", height = "1000px")
  ),
  tags$a(href = "https://github.com/kennardmah/data-product-vision-zero", target = "_blank", class = "contribute-github",
         tags$img(src = "github-cycling.png", class = "github-image", title = "Contribute through our GitHub!"),
         tags$span("Contribute and Share Your Thoughts on our GitHub!", class = "github-text")
  )
)

# Server Function
server <- function(input, output, session) {
  # IMPORTANT: kills the process when closing the app
  session$onSessionEnded(function() { stopApp() })
  
  thematic::thematic_shiny()
  
  # Show the modal dialog when the app starts
  showModal(modalDialog(
    title = HTML('Welcome to    <img src="logo.png" style="height: 50px; width: auto;"/>'),
    HTML("Explore bike crash data and infrastructure in Boston.<br><br> 
    Use the checkboxes to toggle between crash data, bike lanes and pollution metrics.<br><br>
    Disclaimer: There may be discreprencies between PEDAL's data and
    real-world observations. PEDAL is still under development and is
    not resposible for any insights drawn. <br><br> By pressing OK, you agree to this."),
    
    easyClose = TRUE, # When true, clicking outside the popup will close it
    # Add an 'OK' button to the modal dialog, when clicked the popup is closed
    footer = modalButton("OK!")
  ))
  
  output$map <- renderLeaflet({
    # Defines coordinates for the center of Boston and as needed for aesthetics
    boston_lat <- 42.3150
    boston_long <- -71.0589
    zoom_level <- 12  # Adjust this to zoom in/out
    
    map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = boston_long, lat = boston_lat, zoom = zoom_level)
    
    if (input$dataLayer == "bCrash") {
      map <- map %>% addCircleMarkers(data = accidents_df, ~long, ~lat,
                                      popup = ~as.character(dispatch_ts),
                                      color = "red", fillOpacity = 0.2,
                                      weight = 0, radius = 3
      )
    } else if (input$dataLayer == "bBikeLane") {
      map <- map %>% addPolylines(data = filtered_bike_lanes, color = ~laneType,
                                  weight = 3, opacity = 0.7
      )
    } else if (input$dataLayer == "bPM2.5") {
      map <- map %>% addPolygons(data = final_data_sf,
                                 fillColor = ~colorNumeric(palette = c("cyan", "navy"), domain = c(0,75))(pm2_5_level),  # nolint: line_length_linter.
                                 weight = 2,
                                 opacity = 0.3,
                                 color = "white",
                                 dashArray = "",
                                 fillOpacity = 0.5, 
                                 highlightOptions = highlightOptions(weight = 5,
                                                                     opacity = 0.9,  # nolint: line_length_linter.
                                                                     color = "yellow",  # nolint: line_length_linter.
                                                                     dashArray = "1",  # nolint: line_length_linter.
                                                                     fillOpacity = 1,  # nolint: line_length_linter.
                                                                     bringToFront = TRUE),  # nolint: line_length_linter.
                                 label = ~paste(name, "PM2.5 Level:", pm2_5_level, "μg/m^3"),  # nolint: line_length_linter.
                                 labelOptions = labelOptions(
                                   style = list("font-weight" = "normal", padding = "3px 8px"),
                                   textsize = "15px",
                                   direction = "auto"),
                                 smoothFactor = 0.5,
                                 group = "Neighborhoods"
      )
    } else if (input$dataLayer == "bNO2") {
      map <- map %>% addPolygons(data = final_data_sf,
                                 fillColor = ~colorNumeric(palette = c("green", "red"), domain = c(0,200))(no2_level),
                                 weight = 2,
                                 opacity = 0.3,
                                 color = "white",
                                 dashArray = "",
                                 fillOpacity = 0.5, 
                                 highlightOptions = highlightOptions(
                                   weight = 5,
                                   opacity = 0.9,
                                   color = "yellow",
                                   dashArray = "1",
                                   fillOpacity = 1,
                                   bringToFront = TRUE),
                                 label = ~paste(name, "NO2 Level:", no2_level, "μg/m^3"),
                                 labelOptions = labelOptions(
                                   style = list("font-weight" = "normal", padding = "3px 8px"),
                                   textsize = "15px",
                                   direction = "auto"),
                                 smoothFactor = 0.5,
                                 group = "Neighborhoods"
      )
    }
    
    map
  })
}

# Create Shiny App
shinyApp(ui, server)
