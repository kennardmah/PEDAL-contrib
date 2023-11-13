# Install and load packages
if (!require("leaflet")) install.packages("leaflet")
if (!require("osrm")) install.packages("osrm")

library(leaflet)
library(osrm)

# Define your API endpoint
osrm_endpoint <- "http://router.project-osrm.org/route/v1/driving"

# Function to get route information
get_route_information <- function(start_coords, end_coords) {
  route <- osrmRoute(
    osrm = osrm_endpoint,
    src = start_coords,
    dst = end_coords,
    format = "sf"
  )
  route
}

# Function to extract street names from route segments
extract_street_names <- function(route_sf) {
  street_names <- route_sf$name
  unique(street_names)
}

# Example usage
start_coords <- c(-71.0656, 42.3594)  # Boston Common
end_coords <- c(-71.0973, 42.3464)    # Fenway Park

route_sf <- get_route_information(start_coords, end_coords)
street_names <- extract_street_names(route_sf)

print(paste("Streets on the route:", paste(street_names, collapse = ", ")))

# Create a leaflet map
map <- leaflet() %>%
  addTiles() %>%
  addPolylines(
    data = route_sf,
    color = "blue",
    weight = 2
  ) %>%
  addMarkers(lng = start_coords[1], lat = start_coords[2], popup = "Start") %>%
  addMarkers(lng = end_coords[1], lat = end_coords[2], popup = "End")

# Display the map
map
