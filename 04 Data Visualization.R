# Load libraries
library(tidyverse)

# Data acquisition 
covid_data_tbl <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")

# Filter out rows with missing values
covid_data_tbl <- covid_data_tbl %>%
  filter(!is.na(continent) & !is.na(date) & !is.na(total_cases))

# Filter for specific countries
countries <- c("France", "Germany", "Spain", "United Kingdom", "United States")

# Filter for Europe and specific countries, then summarize
europe_data <- covid_data_tbl %>%
  filter(continent == "Europe" & location %in% countries) %>%
  group_by(date) %>%
  summarize(total_cases = sum(total_cases, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(total_cases))

# Filter for individual countries
country_data <- covid_data_tbl %>%
  filter(location %in% countries) %>%
  group_by(location, date) %>%
  summarize(total_cases = max(total_cases, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(total_cases))

# Visualization with ggplot2
ggplot() +
  geom_line(data = europe_data, aes(x = date, y = total_cases, color = "Europe"), size = 1.2) +
  geom_line(data = country_data, aes(x = date, y = total_cases, color = location), size = 1) +
  scale_color_manual(values = c("Europe" = "blue", "France" = "red", "Germany" = "green", 
                                "Spain" = "purple", "United Kingdom" = "orange", "United States" = "black")) +
  scale_x_date(limits = as.Date(c("2020-01-01", "2023-12-31")), date_labels = "%Y-%m", date_breaks = "6 months") +
  labs(title = "Cumulative Covid-19 Cases Over Time", x = "Date", y = "Total Cases", color = "Location") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_blank()
  )



#------------------------------------------------
#
-----------------------------
  
  
  # Load libraries
  library(tidyverse)
library(maps)
library(viridis)

# Data acquisition 
covid_data_tbl <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")

# Filter out rows with missing values
covid_data_tbl <- covid_data_tbl %>%
  filter(!is.na(continent) & !is.na(location) & !is.na(date) & !is.na(total_cases) & !is.na(total_deaths) & !is.na(population))

# Calculate mortality rate
covid_data_tbl <- covid_data_tbl %>%
  mutate(mortality_rate = total_deaths / population * 1000000) # Mortality rate per million people

# Summarize the latest data for each location
latest_data <- covid_data_tbl %>%
  group_by(location) %>%
  filter(date == max(date)) %>%
  ungroup()

# Merge with world map data
world_map <- map_data("world")
world_covid_data <- left_join(world_map, latest_data, by = c("region" = "location"))

# Filter out missing values and select necessary columns
world_covid_data <- world_covid_data %>%
  filter(!is.na(mortality_rate)) %>%
  select(long, lat, group, region, mortality_rate)

# Visualization with geom_polygon
ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
               fill = "white", color = "black", size = 0.1) +
  geom_polygon(data = world_covid_data, aes(x = long, y = lat, group = group, fill = mortality_rate),
               color = "black", size = 0.1) +
  scale_fill_viridis_c(name = "Mortality Rate (per million)",
                       na.value = "grey90",
                       labels = scales::comma) +
  labs(title = "Global Mortality Rate (COVID-19)",
       fill = "Mortality Rate (per million)",
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

#--------------------------------------------------------------
 