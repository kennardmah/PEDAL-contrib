# this file is for cleaning the accident data

# Load packages
library(data.table)

# Loads and prepares bike accident data
df <- fread("dataframe/pre-processed/vision-zero-crash.csv")
bike_df <- df[df$mode_type == "bike"]
fwrite(bike_df, "dataframe/post-processed/vision-zero-crash.csv")
