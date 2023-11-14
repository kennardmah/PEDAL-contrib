# Install and load required packages
if (!requireNamespace("osrm", quietly = TRUE)) {
  install.packages("osrm")
}
if (!requireNamespace("leaflet", quietly = TRUE)) {
  install.packages("leaflet")
}

library(osrm)
library(leaflet)
library(data.table)

# Function to get route information
get_cycling_route <- function(start_point, end_point) {
  # Get route information
  route <- osrmRoute(src = start_point, dst = end_point, overview = "full")
  # Extract route details
  route_data <- route$routes[[1]]$legs[[1]]$steps
  route_coords <- lapply(route_data, function(x) x$geometry$coordinates)
  route_coords <- do.call(c, route_coords)
  # Extract road names
  road_names <- sapply(route_data, function(x) x$name)
  # Create a data.table with route details
  route_dt <- data.table(
    latitude = route_coords[, 2],
    longitude = route_coords[, 1],
    road_name = road_names
  )
  return(route_dt)
}

# Example usage
start_point <- c(-71.0589, 42.3601)  # Boston Common
end_point <- c(-71.0921, 42.3388)    # Fenway Park

route_data <- get_cycling_route(start_point, end_point)

# Print the data.table
print(route_data)

# Visualize the route on a leaflet map
leaflet(route_data) %>%
  addTiles() %>%
  addPolylines(lng = ~longitude, lat = ~latitude, color = "blue", weight = 2) %>%
  addMarkers(lng = c(start_point[1], end_point[1]), lat = c(start_point[2], end_point[2]), label = c("Start", "End"))
