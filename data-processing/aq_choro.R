# Load necessary libraries
library(sf)
library(leaflet)
library(httr)
library(jsonlite)
library(dplyr)
library(RColorBrewer)

# API Key - replace with your actual API key
api_key <- "9cf634461389fa0f5ab332e7c49f22ad"

# Path to your GeoJSON file
file_path <- "dataframe/pre-processed/Boston_Neighborhood_Boundaries_approximated_by_2020_Census_Block_Groups.geojson"

# Read the GeoJSON file
geo_data <- st_read(file_path)

# Extract only the name and geometry
processed_data <- geo_data %>%
  select(blockgr2020_ctr_neighb_name, geometry)

# Calculate centroids of the geometries
centroids <- st_centroid(processed_data$geometry)

# Extract longitude and latitude from centroids
centroid_data <- data.frame(
  name = processed_data$blockgr2020_ctr_neighb_name,
  geomtery = processed_data$geometry,
  centre_lon = st_coordinates(centroids)[, 1],
  centre_lat = st_coordinates(centroids)[, 2]
)

print(centroid_data)

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
      centre_lat = lat,
      centre_lon = lon,
      pm2_5_level = data$list$components$pm2_5,  # Example for PM2.5 levels
      no2_level = data$list$components$no2  # for NO2 levels
    )
    return(processed_data)
  } else {
    # Return an empty dataframe or handle the error appropriately
    return(data.frame(latitude = numeric(0), longitude = numeric(0), pollution_level = numeric(0)))
  }
}

# Fetches AQ data for all the neighborhood centroid points
fetch_pollution_data_all <- function(centroid_data, api_key) {
  all_data <- lapply(1:nrow(centroid_data), function(i) {
    fetch_pollution_data(centroid_data$centre_lat[i], centroid_data$centre_lon[i], api_key)
  })
  do.call(rbind, all_data)
}

# Call the fetch_pollution_data_all function instead of fetch_pollution_data
air_quality_results <- fetch_pollution_data_all(centroid_data, api_key)

# Merge the data
merged_final_data <- merge(centroid_data, air_quality_results, by = c("centre_lat", "centre_lon"))

# Convert to spatial dataframe to allow for map visualisation
final_data_sf <- st_as_sf(merged_final_data, sf_column_name = "geometry")

#Sort alphabetically by neighborhood name
final_data_sf <- final_data_sf %>%
  arrange(name)

map <- leaflet(final_data_sf) %>%
  addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(noWrap = TRUE)) %>%
  addPolygons(
    fillColor = ~colorNumeric(palette = c("#00FFFF", "#0439FF"), domain = c(0,20))(pm2_5_level),
    weight = 2,
    opacity = 0.3,
    color = "white",
    dashArray = "",
    fillOpacity = 0.7, 
    highlightOptions = highlightOptions(
      weight = 5,
      opacity = 0.9,
      color = "orange",
      dashArray = "1",
      fillOpacity = 1,
      bringToFront = TRUE),
    label = ~paste(name, "PM2.5 Level:", pm2_5_level, "μg/m^3"),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"),
    smoothFactor = 0.5,
    group = "Neighborhoods"
    )

map
