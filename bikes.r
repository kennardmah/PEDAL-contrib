# Install and load packages
if (!require("leaflet")) install.packages("leaflet")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("data.table")) install.packages("data.table")

library(leaflet)
library(ggplot2)
library(data.table)

# Load dataframe for accidents
df <- fread("accident-data/vision-zero-crash.csv")

# Filter to cyclists
bike_df <- df[mode_type == "bike"]

# Create a leaflet map with clustering
map <- leaflet(bike_df) %>%
  addTiles() %>%
  addMarkers(~long, ~lat, popup = ~as.character(`dispatch_ts`), clusterOptions = markerClusterOptions())

# Display the map
map