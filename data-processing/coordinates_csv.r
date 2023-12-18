# Install and load the sf package
library(sf)
library(leaflet)

file_path <- "dataframe/pre-processed/Boston_Neighborhood_Boundaries_approximated_by_2020_Census_Block_Groups.geojson"

# Read the GeoJSON file
geo_data <- st_read(file_path)

# Extract only the name and geometry
processed_data <- geo_data[c("blockgr2020_ctr_neighb_name", "geometry")]
# st_write(processed_data, "dataframe/post-processed/name-coordinates.geojson", driver = "GeoJSON")

# Calculate centroids of the geometries
centroids <- st_centroid(processed_data$geometry)
# Extract longitude and latitude from centroids
lon <- st_coordinates(centroids)[,1]
lat <- st_coordinates(centroids)[,2]
# Create a data frame with location names and their centroid coordinates
centroid_data <- data.frame(
  blockgr2020_ctr_neighb_name = processed_data$blockgr2020_ctr_neighb_name,
  longitude = lon,
  latitude = lat
)

# Save CSV file (if haven't)
# write.csv(centroid_data, "dataframe/post-processed/name-to-coordinates.csv", row.names = FALSE)


### TEST BY MAPPING ON LEAFLET ###

# Calculate centroids and add them to the geo_data
processed_data$longitude <- st_coordinates(st_centroid(processed_data$geometry))[,1]
processed_data$latitude <- st_coordinates(st_centroid(processed_data$geometry))[,2]

# Create a leaflet map with a minimal base map
map <- leaflet(processed_data) %>%
  addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(noWrap = TRUE)) %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.6,
              highlight = highlightOptions(weight = 3, color = "#666", fillOpacity = 0.7)) %>%
  addCircleMarkers(lng = ~longitude, lat = ~latitude, 
                   radius = 5, color = "#007bff", stroke = FALSE, fillOpacity = 1)

map