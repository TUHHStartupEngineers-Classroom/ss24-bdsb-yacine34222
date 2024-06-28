# install.packages("plotly")

library(tidyverse)
library(tidyquant)
library(broom)
library(umap)

# STOCK PRICES
sp_500_prices_tbl <- read_rds("C:/Users/Yacine Azouz/OneDrive/Documents/GitHub/ss24-bdsb-yacine34222/Business Decisions with Machine Learning/Business Decisions with Machine Learning/sp_500_prices_tbl.rds")
sp_500_prices_tbl


# SECTOR INFORMATION
sp_500_index_tbl <- read_rds("C:/Users/Yacine Azouz/OneDrive/Documents/GitHub/ss24-bdsb-yacine34222/Business Decisions with Machine Learning/Business Decisions with Machine Learning/sp_500_index_tbl.rds")
sp_500_index_tbl


sp_500_prices_tbl %>% glimpse()

# Apply your data transformation skills!

sp_500_daily_returns_tbl <- sp_500_prices_tbl %>%
  select(symbol, date, adjusted) %>%
  filter(date >= as.Date("2018-01-01")) %>%
  group_by(symbol) %>%
  mutate(lag_adjusted = lag(adjusted)) %>%
  filter(!is.na(lag_adjusted)) %>%
  mutate(pct_return = (adjusted - lag_adjusted) / lag_adjusted) %>%
  select(symbol, date, pct_return)

sp_500_daily_returns_tbl



sp_500_daily_returns_tbl <- read_rds("C:/Users/Yacine Azouz/OneDrive/Documents/GitHub/ss24-bdsb-yacine34222/Business Decisions with Machine Learning/Business Decisions with Machine Learning/sp_500_daily_returns_tbl.rds")
sp_500_daily_returns_tbl


# Convert to User-Item Format

stock_date_matrix_tbl <- sp_500_daily_returns_tbl %>%
  spread(key = date, value = pct_return, fill = 0)

stock_date_matrix_tbl

stock_date_matrix_tbl <- read_rds("C:/Users/Yacine Azouz/OneDrive/Documents/GitHub/ss24-bdsb-yacine34222/Business Decisions with Machine Learning/Business Decisions with Machine Learning/stock_date_matrix_tbl.rds")



# Create kmeans_obj for 4 centers

kmeans_obj <- stock_date_matrix_tbl %>%
  select(-symbol) %>%
  kmeans(centers = 4, nstart = 20)


# Apply glance() to get the tot.withinss

kmeans_obj %>%
  glance() %>%
  select(tot.withinss)



kmeans_mapper <- function(center = 3) {
  stock_date_matrix_tbl %>%
    select(-symbol) %>%
    kmeans(centers = center, nstart = 20)
}






