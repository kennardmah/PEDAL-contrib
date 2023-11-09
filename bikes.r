install.packages('httr')

library(httr)

city <- 'Boston'
base_url <- "https://data.boston.gov/api/3/action/datastore_search"
params <- list(
    resource_id = 'e4bfe397-6bfc-49c5-9367-c879fac7401d'
    limit = 5
)

response <- GET(url = base_url, query = params)

# Check if the request was successful
if (http_status(response)$category == "success") {
  data <- content(response, as = "text")  # Extract content from the response object
  data <- fromJSON(data)  # Parse the JSON content
  
  print(data$result$records)  # Print the results
} else {
  print(paste("Failed to retrieve data:", http_status(response)$reason))
}
