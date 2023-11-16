# Load required libraries
library(leaflet)
library(sf)
library(dplyr)
library(data.table)
library(ggplot2)

# Load and prepare bike accident data
df <- fread("dataframe/vision-zero-crash.csv")
bike_df <- df[df$mode_type == "bike"]
bike_df$isDark <- ifelse(as.integer(format(bike_df$dispatch_ts, "%H")) >= 15 | as.integer(format(bike_df$dispatch_ts, "%H")) < 6, 1, 0)

# Load and filter infrastructure data
bike_lanes_geojson <- sf::read_sf("dataframe/Existing_Bike_Network_2023.geojson")
filtered_bike_lanes <- bike_lanes_geojson %>% filter(!(ExisFacil %in% c("WALK", "PED")))

# Create a combined map
combined_map <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap tiles
  
  # Add infrastructure layers
  addPolylines(data = filtered_bike_lanes, color = "Green", weight = 3, opacity = 0.7, group = ~ExisFacil) %>%

  # Add accident markers for dark conditions
  addCircleMarkers(data = bike_df[bike_df$isDark == 1], ~long, ~lat, popup = ~as.character(dispatch_ts), group = 'Dark Conditions', color = 'red', fillOpacity = 0.1, weight = 0, radius = 3) %>%

  # Add accident markers for light conditions
  addCircleMarkers(data = bike_df[bike_df$isDark == 0], ~long, ~lat, popup = ~as.character(dispatch_ts), group = 'Light Conditions', color = 'red', fillOpacity = 0.1, weight = 0, radius = 3)

# Display the combined map
combined_map
