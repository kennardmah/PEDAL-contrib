# upload library
library(data.table)

# Import the CSV file using fread
pollution_data <- fread("dataframe/boston-air-quality.csv")
# Remove the columns
pollution_data[, (c("pm10", "o3", "so2", "co")) := NULL]

# Convert the 'date' column to Date type
pollution_data[, date := as.Date(date, format = "%Y/%m/%d")]

# Sort the data table by 'date'
pollution_data <- pollution_data[order(date)]

# Save the sorted data table to a new CSV file
fwrite(pollution_data, "dataframe/sorted_boston_air_quality.csv")
