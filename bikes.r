# # Install and load packages
# install.packages(c("leaflet", "ggplot2", "data.table"))

library(leaflet)
library(ggplot2)
library(data.table)

# Load dataframe for accidents
df <- fread("dataframe/vision-zero-crash.csv")

# Filter to cyclists
bike_df <- df[mode_type == "bike"]

# Add the isDark column
bike_df[, isDark := ifelse(as.integer(format(dispatch_ts, "%H")) >= 15 | as.integer(format(dispatch_ts, "%H")) < 6, 1, 0)]
dark_df <- bike_df[isDark == 1]
light_df <- bike_df[isDark == 0]

# Create a leaflet map
accidents <- leaflet(bike_df) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles

  # Add markers for dark conditions
  addCircleMarkers(data = bike_df[isDark == 1], ~long, ~lat, 
                   popup = ~as.character(dispatch_ts), 
                   group = 'Dark Conditions', color = 'red', fillOpacity = 0.1, weight = 0, radius = 3) %>%

  # Add markers for light conditions
  addCircleMarkers(data = bike_df[isDark == 0], ~long, ~lat, 
                   popup = ~as.character(dispatch_ts), 
                   group = 'Light Conditions', color = 'red', fillOpacity = 0.1, weight = 0, radius = 3) %>%

  # Add layer control
  addLayersControl(overlayGroups = c('Dark Conditions', 'Light Conditions'),
                   options = layersControlOptions(collapsed = FALSE))

# Display the map
accidents
