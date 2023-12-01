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
  
  # Custom CSS to adjust the layout
  tags$head(
    tags$style(HTML("
      /* Full width layout adjustments */
      .container-fluid {
        padding-left: 0;
        padding-right: 0;
        padding-top: 0;
        margin-top: 20;
      }
      /* Adjust width and height of sidebar to fill its parent */
      .sidebar {
        padding: 0;
        border-right: 1px solid #ddd;
        height: calc(100vh - 50px); /* Set height to full view height minus title bar */
      }
      .main {
        padding: 0;
        height: calc(100vh - 50px); /* Set height to full view height minus title bar */
      }
      /* Title bar styling */
      .title-bar {
        background-color: #06133E;
        color: white;
        padding: 10px;
        font-size: 24px;
        text-align: center;
        height: 50px; /* Set a fixed height for the title bar */
      }
    "))
  ),

  # Title bar
  div(class = "title-bar",
    "PEDAL: Visualizations for Cyclist Safety in Boston"
  ),

  # Main layout with sidebar and map
  div(class = "container-fluid",
    div(class = "row",
      div(class = "col-sm-3 sidebar",
        div(class = "sidebar-content",
          wellPanel(
            checkboxInput("bCrash", "Crashes", value = TRUE),
            checkboxInput("bBikeLane", "Bike Lanes", value = FALSE)
          )
        )
      ),
      div(class = "col-sm-9 main",
        leafletOutput("map")
      )
    )
  )
)


# Defines server logic
server <- function(input, output, session) {
  # IMPORTANT: kills the process when closing the app
  session$onSessionEnded(function() { stopApp() })

  thematic::thematic_shiny()

  # Loads and prepares bike accident data
  df <- fread("dataframe/vision-zero-crash.csv")
  bike_df <- df[df$mode_type == "bike"]

  # Loads and filters infrastructure data
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
    # Defines coordinates for the center of Boston and as needed for aesthetics
    boston_lat <- 42.3550
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

    # Returns the modified map
    map
  })
}

# Creates Shiny app
shinyApp(ui, server)