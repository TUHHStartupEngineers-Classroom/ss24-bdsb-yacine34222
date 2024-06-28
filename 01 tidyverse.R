
  
  
  
  
  
  
  
  
  
  # Load libraries
  library(tidyverse)
library(readxl)

# Load data
bikes_tbl <- read_excel("C:/Users/Yacine Azouz/OneDrive/Desktop/s4/Business and mangement/Business Data Science Basics/1/ds_data/ds_data/01_bike_sales/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel("C:/Users/Yacine Azouz/OneDrive/Desktop/s4/Business and mangement/Business Data Science Basics/1/ds_data/ds_data/01_bike_sales/01_raw_data/orderlines.xlsx")
bikeshops_tbl <- read_excel("C:/Users/Yacine Azouz/OneDrive/Desktop/s4/Business and mangement/Business Data Science Basics/1/ds_data/ds_data/01_bike_sales/01_raw_data/bikeshops.xlsx")

# Join tables
bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

# Wrangle data
bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
  separate(col = category, into = c("category_1", "category_2", "category_3"), sep = " - ") %>%
  mutate(total_price = price * quantity) %>%
  select(-...1, -gender) %>%
  select(-ends_with(".id")) %>%
  bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>%
  select(order.id, contains("order"), contains("model"), contains("category"),
         price, quantity, total_price,
         everything()) %>%
  rename(bikeshop = name) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))

# Split location into state and city columns
if ("location" %in% colnames(bike_orderlines_wrangled_tbl)) {
  bike_orderlines_wrangled_tbl <- bike_orderlines_wrangled_tbl %>%
    separate(location, into = c("city", "state"), sep = ",", extra = "merge")
}

# Ensure order_date is parsed as a date
bike_orderlines_wrangled_tbl <- bike_orderlines_wrangled_tbl %>%
  mutate(order_date = as.Date(order_date))

# Calculate total sales by state
sales_by_loc_tbl <- bike_orderlines_wrangled_tbl %>%
  group_by(state) %>%
  summarize(total_sales = sum(total_price, na.rm = TRUE))

# Bar plot of sales by state
sales_by_loc_tbl %>%
  ggplot(aes(x = reorder(state, total_sales), y = total_sales)) +
  geom_bar(stat = "identity") +
  labs(title = "Sales by State", x = "State", y = "Total Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Calculate sales by state and year
sales_by_loc_year_tbl <- bike_orderlines_wrangled_tbl %>%
  mutate(year = lubridate::year(order_date)) %>%
  group_by(state, year) %>%
  summarize(total_sales = sum(total_price, na.rm = TRUE))

# Faceted bar plot of sales by state and year
sales_by_loc_year_tbl %>%
  ggplot(aes(x = year, y = total_sales)) +
  geom_bar(stat = "identity") +
  facet_wrap(~state, ncol = 3, scales = "free_y") +
  labs(title = "Sales by State and Year", x = "Year", y = "Total Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
