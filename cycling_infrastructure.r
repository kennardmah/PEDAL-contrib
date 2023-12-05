# Load required libraries
library(leaflet)
library(sf)
library(dplyr)
library(data.table)

# Loads and filters infrastructure data
bike_lanes_geojson <- sf::read_sf(
  "dataframe/pre-processed/Existing_Bike_Network_2023.geojson")
filtered_bike_lanes <- bike_lanes_geojson %>%
  filter(!(ExisFacil %in% c("WALK", "PED")))

# Add a new column 'laneType' for cycling lane type and select desired columns
filtered_bike_lanes <- filtered_bike_lanes %>%
  mutate(laneType = case_when(
    ExisFacil %in% c("BL", "SBLBL") ~ "NPSNB",
    ExisFacil %in% c("BL-PEAKBUS", "BLSL", "CFBL",
                     "SBLSL", "SLM", "SLMTC", "SUP", "SUPN", "SUPM") ~ "S",
    ExisFacil == "BFBL" ~ "NPSB",
    ExisFacil %in% c("SBL", "CFSBL") ~ "PS",
    TRUE ~ NA_character_ 
  )) %>%
  filter(!is.na(laneType)) %>% # remove NA row (there is only 1)
  # remove un-used columns
  # select(STREET_NAM, Shape_Leng, Shape__Length, geometry, laneType)

# Output this to post-processed
sf::st_write(filtered_bike_lanes,
             "dataframe/post-processed/Existing_Bike_Network_2023.geojson")
