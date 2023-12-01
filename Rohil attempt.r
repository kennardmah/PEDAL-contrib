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
    # Defines coordinates for the center of Boston
    boston_lat <- 42.3601
    boston_long <- -71.0589
    zoom_level <- 12  # Adjust this to zoom in/out

    # A base Leaflet map centered on Boston
    map <- leaflet() %>%
           addTiles() %>%
           setView(lng = boston_long, lat = boston_lat, zoom = zoom_level)


    # Conditionally adds bike lanes on check box of Bike Lanes
    if(input$bBikeLane) {
      map <- map %>% addPolylines(data = filtered_bike_lanes, color = "Green",
                                  weight = 3, opacity = 0.7, group = ~ExisFacil)
    }

    # Conditionally adds crash locations on check box of Crashes
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