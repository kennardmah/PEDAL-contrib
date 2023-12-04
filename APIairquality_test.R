# TESTING AIR QUALITY API CALLS

library(httr)
library(jsonlite)

# API Key - replace with your actual API key
api_key <- "KEY"

# Center coordinates
center_lat <- 42.3150
center_lon <- -71.0589
num_points <- 5  # for a 5x5 grid
offset <- 0.10  # Adjust as needed for distance between points

# Creates grid of 5 by 5 locations around center coordinates
generate_grid_coordinates <- function(center_lat, center_lon, num_points, offset) {
  grid <- expand.grid(lat = seq(from = center_lat - offset * (num_points / 2), 
                                to = center_lat + offset * (num_points / 2), 
                                by = offset),
                      lon = seq(from = center_lon - offset * (num_points / 2), 
                                to = center_lon + offset * (num_points / 2), 
                                by = offset))
  return(grid)
}

# Generate grid coordinates
coordinates <- generate_grid_coordinates(center_lat, center_lon, num_points, offset)

# Function to fetch and process air pollution data for a single point
fetch_pollution_data <- function(lat, lon, api_key) {
  url <- paste0("http://api.openweathermap.org/data/2.5/air_pollution?lat=", 
                lat, "&lon=", lon, "&appid=", api_key)
  response <- GET(url)
  data <- fromJSON(rawToChar(response$content))
  
  # Print the entire data structure
  print(data)
  
  # Check if the expected data is present
  if(!is.null(data$list) && !is.null(data$list$components) && !is.null(data$list$components$pm2_5)) {
    # Process the data and return a dataframe
    processed_data <- data.frame(
      latitude = lat,
      longitude = lon,
      pm2_5_level = data$list$components$pm2_5,  # Example for PM2.5 levels
      no2_level = data$list$components$no2  # for NO2 levels
    )
    return(processed_data)
  } else {
    # Return an empty dataframe or handle the error appropriately
    return(data.frame(latitude = numeric(0), longitude = numeric(0), pollution_level = numeric(0)))
  }
}

# Function to fetch and process air pollution data for all specified coordinates 
fetch_pollution_data_all <- function(coordinates, api_key) {
  all_data <- lapply(1:nrow(coordinates), function(i) {
    fetch_pollution_data(coordinates$lat[i], coordinates$lon[i], api_key)
  })
  do.call(rbind, all_data)
}

# Fetch and print data for all locations
fetched_data <- fetch_pollution_data_all(coordinates, api_key)
print("Processed Data for All Locations:")
print(fetched_data)
