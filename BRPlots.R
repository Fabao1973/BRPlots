library(tidyverse)
library(tidyquant)
library(scales)
# Import data | link to data: https://odn.data.socrata.com/Education/ODN-Education/8apn-rhyh
edu_census_data_rwa_tbl <- read.csv("ODN_Education.csv")
# Glimpse data
edu_census_data_rwa_tbl %>% glimpse()
edu_census_data_rwa_tbl <- edu_census_data_rwa_tbl[,2:6]
## FILTER DATA FOR PLOTTING
# Setup variables for filter + for using in plot later
year_f <- 2018
nation <- "United States"
county <- "Los Angeles County"
# Data prep
edu_census_filtered_tbl <- edu_census_data_rwa_tbl %>% 
  # Filter data to year and areas of interest
  filter(year == year_f,
         name == nation | # OR
           str_detect(name, county))
# View data
edu_census_filtered_tbl %>% glimpse()
# Generic ggplot()
edu_census_filtered_tbl %>% 
  ggplot(aes(x = variable, y = value, fill = name)) +
  geom_col()
## BUSINESS READY PLOTS
# Data manipulation
# Step 1 - Manipulation data
data_manipulated_tbl <- edu_census_filtered_tbl %>% 
  # Selecting columns to focus on
  select(name, variable, value) %>% 
  # Tidy up variable names
  mutate(variable = str_replace(variable, "percent_", ""),
         variable = str_replace_all(variable, "_", " "),
         variable = str_to_title(variable)) %>% 
  # Convert value to a percent (ratio)
  mutate(pct = value / 100) %>% 
  # Format % Text
  mutate(pct_text = scales::percent(pct, accuracy = 0.1)) %>% 
  #Select final columns for plotting
  select(name, variable, contains("pct"))
data_manipulated_tbl <- data_manipulated_tbl %>% as_tibble()
data_manipulated_tbl <- data_manipulated_tbl[1:10,]
# Data visualization
# Step 2 - Visualize data
data_visualized_plot <- data_manipulated_tbl %>% 
  #Setup ggplot() canvas for plotting
  ggplot(aes(x = variable, y = pct, fill = name)) +
  #Geometrics
  geom_col() +
  geom_label(aes(label = pct_text), fill = "white", hjust = "center") +
  #Facet: splits plot multiple plots by categorical feature
  facet_wrap(~ name) +
  #Flip coordinates for readable variable names
  coord_flip() +
  #Formatting
  theme_tq() +
  scale_fill_tq() +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1.0)) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold")) +
  labs(tittle = str_glue("Comparison of Educational Attainment ({year_f})"),
       caption = "Census Data",
       x = "", y = "")
data_visualized_plot

# Delete big data
rm(edu_census_data_rwa_tbl)










