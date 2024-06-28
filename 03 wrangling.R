library(vroom)
library(dplyr)


col_types <- list(
  id = col_character(),
  date = col_date("%Y-%m-%d"),
  num_claims = col_double()
)

patent_tbl <- vroom(
  file       = "C:/Users/Yacine Azouz/OneDrive/Documents/GitHub/ss24-bdsb-yacine34222/data wrangling/Patent_data_reduced/patent.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

library(dplyr)


top_10_companies <- patent_tbl %>%
  filter(!is.na(id)) %>%
  group_by(id) %>%
  summarise(num_patents = n()) %>%
  arrange(desc(num_patents)) %>%
  head(10)

top_10_companies
library(data.table)
library(dplyr)

# Convert patent_tbl to data.table
setDT(patent_tbl)

# Get top 10 companies with the most patents
top_10_companies <- patent_tbl[!is.na(id)][, .(num_patents = .N), by = id][order(-num_patents)][1:10]

top_10_companies

#-------------------------------------------------------------------------------


library(dplyr)

# Filter patents granted in August 2014
patents_aug_2014 <- patent_tbl %>%
  filter(!is.na(date)) %>%
  filter(year(date) == 2014, month(date) == 8)

# Count patents for each company
patents_aug_2014_count <- patents_aug_2014 %>%
  group_by(id) %>%
  summarise(num_patents = n()) %>%
  arrange(desc(num_patents))

# Top 10 companies with the most patents granted in August 2014
top_10_aug_2014 <- head(patents_aug_2014_count, 10)

top_10_aug_2014
library(data.table)

# Convert patent_tbl to data.table
setDT(patent_tbl)

# Filter patents granted in August 2014
patents_aug_2014 <- patent_tbl[!is.na(date) & year(date) == 2014 & month(date) == 8]

# Count patents for each company
patents_aug_2014_count <- patents_aug_2014[, .(num_patents = .N), by = id][order(-num_patents)]

# Top 10 companies with the most patents granted in August 2014
top_10_aug_2014 <- patents_aug_2014_count[1:10]

top_10_aug_2014


#-----------------------------------------------------

library(dplyr)

# Filter patents for the top5 companies with the most patents 
top_5_companies <- patent_tbl %>%
  filter(!is.na(id)) %>%
  group_by(id) %>%
  summarise(num_patents = n()) %>%
  arrange(desc(num_patents)) %>%
  head(5)

top_5_company_ids <- top_5_companies$id

# Filter patents for the top 10 companies
top_5_patents <- patent_tbl %>%
  filter(id %in% top_5_company_ids)

# Extract tech main class from id column
top_5_patents <- top_5_patents %>%
  mutate(tech_mainclass = substr(id, 1, 3))

# Count patents for each tech main class
tech_main_classes <- top_5_patents %>%
  group_by(id, tech_mainclass) %>%
  summarise(num_patents = n()) %>%
  arrange(id, desc(num_patents)) %>%
  top_n(5, num_patents)

most_innovative_sector <- tech_main_classes$tech_mainclass[which.max(tech_main_classes$num_patents)]

tech_main_classes
#---------------------------------------------------------------------------------------------------------

library(data.table)

# Convert patent_tbl to data.table
setDT(patent_tbl)

# Filter patents for the top 5 companies with the most patents
top_5_companies <- patent_tbl[!is.na(id)][, .(num_patents = .N), by = id][order(-num_patents)][1:5]

top_5_company_ids <- top_5_companies$id

# Filter patents for the top 5 companies
top_5_patents <- patent_tbl[id %in% top_5_company_ids]

# Extract tech main class from id column
top_5_patents[, tech_mainclass := substr(id, 1, 3)]

# Count patents for each tech main class
tech_main_classes <- top_5_patents[, .(num_patents = .N), by = .(id, tech_mainclass)][order(id, -num_patents)][1:5]

most_innovative_sector <- tech_main_classes$tech_mainclass[which.max(tech_main_classes$num_patents)]

tech_main_classes





