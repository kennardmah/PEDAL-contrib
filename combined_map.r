# Load required libraries
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(data.table)
library(ggplot2)

# Define UI
ui <- fluidPage(
  # Reference "www/styles.css"
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  # Title
  div(class = "title-panel",
      tags$h1("CycleCompanion: Interactive Analysis of Cycling for Informed Decision Making")
  ),
  # Sidebar
  div(class = "leaflet-sidebar",
      sidebarPanel(
        HTML("<h4>Description</h4>"),
        textOutput("description")
      )
  ),
  # Underlying Map
  leafletOutput("map")
)

# Define server logic
server <- function(input, output) {

  # Provide descriptive text
  output$description <- renderText({
    "This data product visualizes bike accidents in relation to the existing bike infrastructure. Red markers indicate accidents that occurred in dark conditions, offering insights into potential areas for safety improvements."
  })

  # Load and prepare bike accident data
  df <- fread("dataframe/vision-zero-crash.csv")
  bike_df <- df[df$mode_type == "bike"]
  bike_df$isDark <- ifelse(as.integer(format(bike_df$dispatch_ts, "%H")) >= 15 | as.integer(format(bike_df$dispatch_ts, "%H")) < 6, 1, 0)

  # Load and filter infrastructure data
  bike_lanes_geojson <- sf::read_sf("dataframe/Existing_Bike_Network_2023.geojson")
  filtered_bike_lanes <- bike_lanes_geojson %>% filter(!(ExisFacil %in% c("WALK", "PED")))

  # Create and render the map
  output$map <- renderLeaflet({
    leaflet(filtered_bike_lanes) %>%
      addTiles() %>%
      addPolylines(data = filtered_bike_lanes, color = "Green", weight = 3, opacity = 0.7, group = ~ExisFacil) %>%
      addCircleMarkers(data = bike_df, ~long, ~lat, popup = ~as.character(dispatch_ts), color = 'red', fillOpacity = 0.2, weight = 0, radius = 3)
  })
}

# Create Shiny app
shinyApp(ui, server)
