# Load in required libraries
library(shiny)
library(shinydashboard)
library(leaflet)
library(sf)
library(dplyr)
library(data.table)
library(ggplot2)
library(thematic)

ui <- fluidPage(

  theme = bslib::bs_theme(bootswatch = "flatly"),

  sidebarLayout(
    sidebarPanel(
      br(),
      br(),
      checkboxInput("bCrash", "Crashes", value = FALSE),
      checkboxInput("bBikeLane", "Bike Lanes", value = FALSE),
      br(),
      width = 2
    ),

    mainPanel(
      h2("PEDAL: Bike Crash and Infrastructure Data in Boston"),
      leafletOutput("map", height = 800)
    )
  )
)


# Define server logic
server <- function(input, output, session) {
  # IMPORTANT: kills the process when closing the app
  session$onSessionEnded(function() { stopApp() })

  thematic::thematic_shiny()

  # Load and prepare bike accident data
  df <- fread("dataframe/vision-zero-crash.csv")
  bike_df <- df[df$mode_type == "bike"]

  # Load and filter infrastructure data
  bike_lanes_geojson <- sf::read_sf("dataframe/Existing_Bike_Network_2023.geojson")
  filtered_bike_lanes <- bike_lanes_geojson %>% filter(!(ExisFacil %in% c("WALK", "PED")))

  # Create and render the map
  output$map <- renderLeaflet({
    # Start with a base Leaflet map
    map <- leaflet(filtered_bike_lanes) %>% addTiles()

    # Conditionally add bike lanes
    if(input$bBikeLane) {
      map <- map %>% addPolylines(data = filtered_bike_lanes, color = "Green",
                                  weight = 3, opacity = 0.7, group = ~ExisFacil)
    }

    # Conditionally add crash locations
    if(input$bCrash) {
      map <- map %>% addCircleMarkers(data = bike_df, ~long, ~lat,
                                      popup = ~as.character(dispatch_ts), color = "red",
                                      fillOpacity = 0.2, weight = 0, radius = 3)
    }

    # Return the modified map
    map
  })
}

# Create Shiny app
shinyApp(ui, server)