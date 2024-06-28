# Load necessary libraries
library(httr)
library(jsonlite)
library(tidyverse)

# Define the endpoint for the API call
base_url <- "https://jsonplaceholder.typicode.com/users"

# Make the API request
response <- GET(base_url)

# Check if the request was successful
if (http_status(response)$category == "Success") {
  # Parse the content of the response
  users_data <- content(response, as = "text") %>%
    fromJSON(flatten = TRUE)
  
  # Convert to a tibble for better readability
  users_tbl <- as_tibble(users_data)
  
  # Print the data
  print(users_tbl)
  
  # Display selected columns in a readable format
  selected_users_tbl <- users_tbl %>%
    select(id, name, username, email, address.city, company.name) %>%
    rename(
      UserID = id,
      Name = name,
      Username = username,
      Email = email,
      City = address.city,
      Company = company.name
    )
  
  # Print the selected columns
  print(selected_users_tbl)
  
  # Plot the number of users in each city
  selected_users_tbl %>%
    count(City) %>%
    ggplot(aes(x = reorder(City, n), y = n)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(title = "Number of Users in Each City", x = "City", y = "Number of Users") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
} else {
  print("Failed to fetch users data")
}




#------------------------------------------------------------------------------------------------------


---------------------------------
  library(tidyverse)
library(rvest)

# Set the base URL and category URL
base_url <- "https://www.radon-bikes.de"
category_url <- "https://www.radon-bikes.de/roadbike-gravel/carbon/"

# Get bike URLs for the selected category
category_page <- read_html(category_url)
bike_urls <- category_page %>%
  html_nodes(".product-tile a") %>%
  html_attr("href") %>%
  paste0(base_url, .)

# Function to scrape bike data with error handling
scrape_bike_data <- function(url) {
  bike_page <- read_html(url)
  
  model_name <- bike_page %>%
    html_node("h1") %>%
    html_text(trim = TRUE)
  
  # Attempt to get the price using multiple selectors
  price <- bike_page %>%
    html_node(".product-price__current-value") %>%
    html_text(trim = TRUE)
  
  if (is.na(price)) {
    price <- bike_page %>%
      html_node(".product-price span") %>%
      html_text(trim = TRUE)
  }
  
  if (is.na(price)) {
    price <- bike_page %>%
      html_node(".price-current") %>%
      html_text(trim = TRUE)
  }
  
  price <- price %>%
    gsub("[^0-9,]", "", .) %>%
    gsub(",", ".", .) %>%
    as.numeric()
  
  data.frame(model_name, price)
}

# Scrape data for all bikes in the category
bike_data <- map_df(bike_urls, scrape_bike_data)

# Print the first 10 rows
head(bike_data, 10)



#--------------------
  
  
 