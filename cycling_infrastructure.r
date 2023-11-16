# Load required libraries
library(leaflet)
library(sf)
library(dplyr)

# Read the GeoJSON file
bike_lanes_geojson <- sf::read_sf("dataframe/Existing_Bike_Network_2023.geojson")

# Filter out 'WALK' and 'PED' bike lanes
filtered_bike_lanes <- bike_lanes_geojson %>% 
  filter(!(ExisFacil %in% c("WALK", "PED")))

# Add bike lanes to the map with color based on type
infrastructure <- leaflet(filtered_bike_lanes) %>%
  addTiles() %>%  # Add default OpenStreetMap tiles
  addPolylines(
    color = "Green",
    weight = 3,  # Set line weight to 1
    opacity = 0.7,
    group = ~ExisFacil
  )

# Display the map
infrastructure